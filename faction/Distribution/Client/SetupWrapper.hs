-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.SetupWrapper
-- Copyright   :  (c) The University of Glasgow 2006,
--                    Duncan Coutts 2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  alpha
-- Portability :  portable
--
-- An interface to building and installing Faction packages.
-- If the @Built-Type@ field is specified as something other than
-- 'Custom', and the current version of Faction is acceptable, this performs
-- setup actions directly.  Otherwise it builds the setup script and
-- runs it with the given arguments.

module Distribution.Client.SetupWrapper (
    setupWrapper,
    SetupScriptOptions(..),
    defaultSetupScriptOptions,
  ) where

import Distribution.Client.Types
         ( InstalledPackage )

import qualified Distribution.Simple as Simple
import Distribution.Version
         ( Version(..), VersionRange, anyVersion
         , intersectVersionRanges, orLaterVersion
         , withinRange )
import Distribution.Package
         ( PackageIdentifier(..), PackageName(..), Package(..), packageName
         , packageVersion, Dependency(..) )
import Distribution.PackageDescription
         ( GenericPackageDescription(packageDescription)
         , PackageDescription(..), specVersion
         , BuildType(..), knownBuildTypes )
import Distribution.PackageDescription.Parse
         ( readPackageDescription )
import Distribution.Simple.Configure
         ( configCompiler )
import Distribution.Simple.Compiler
         ( CompilerFlavor(GHC), Compiler, PackageDB(..), PackageDBStack )
import Distribution.Simple.Program
         ( ProgramConfiguration, emptyProgramConfiguration
         , rawSystemProgramConf, ghcProgram )
import Distribution.Simple.BuildPaths
         ( defaultDistPref, exeExtension )
import Distribution.Simple.Command
         ( CommandUI(..), commandShowOptions )
import Distribution.Simple.GHC
         ( ghcVerbosityOptions )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Client.IndexUtils
         ( getInstalledPackages )
import Distribution.Simple.Utils
         ( die, debug, info, factionVersion, findPackageDesc, comparing
         , createDirectoryIfMissingVerbose, rewriteFile, intercalate )
import Distribution.Client.Utils
         ( moreRecentFile, inDir )
import Distribution.Text
         ( display )
import Distribution.Verbosity
         ( Verbosity )

import System.Directory  ( doesFileExist, getCurrentDirectory )
import System.FilePath   ( (</>), (<.>) )
import System.IO         ( Handle )
import System.Exit       ( ExitCode(..), exitWith )
import System.Process    ( runProcess, waitForProcess )
import Control.Monad     ( when, unless )
import Data.List         ( maximumBy )
import Data.Maybe        ( fromMaybe, isJust )
import Data.Char         ( isSpace )

data SetupScriptOptions = SetupScriptOptions {
    useFactionVersion :: VersionRange,
    useCompiler       :: Maybe Compiler,
    usePackageDB      :: PackageDBStack,
    usePackageIndex   :: Maybe PackageIndex,
    useProgramConfig  :: ProgramConfiguration,
    useDistPref       :: FilePath,
    useLoggingHandle  :: Maybe Handle,
    useWorkingDir     :: Maybe FilePath
  }

defaultSetupScriptOptions :: SetupScriptOptions
defaultSetupScriptOptions = SetupScriptOptions {
    useFactionVersion = anyVersion,
    useCompiler       = Nothing,
    usePackageDB      = [GlobalPackageDB, UserPackageDB],
    usePackageIndex   = Nothing,
    useProgramConfig  = emptyProgramConfiguration,
    useDistPref       = defaultDistPref,
    useLoggingHandle  = Nothing,
    useWorkingDir     = Nothing
  }

setupWrapper :: Verbosity
             -> SetupScriptOptions
             -> Maybe PackageDescription
             -> CommandUI flags
             -> (Version -> flags)
             -> [String]
             -> IO ()
setupWrapper verbosity options mpkg cmd flags extraArgs = do
  pkg <- maybe getPkg return mpkg
  let setupMethod = determineSetupMethod options' buildType'
      options'    = options {
                      useFactionVersion = intersectVersionRanges
                                          (useFactionVersion options)
                                          (orLaterVersion (specVersion pkg))
                    }
      buildType'  = fromMaybe Custom (buildType pkg)
      mkArgs libfactionVersion = commandName cmd
                             : commandShowOptions cmd (flags libfactionVersion)
                            ++ extraArgs
  checkBuildType buildType'
  setupMethod verbosity options' (packageId pkg) buildType' mkArgs
  where
    getPkg = findPackageDesc (fromMaybe "." (useWorkingDir options))
         >>= readPackageDescription verbosity
         >>= return . packageDescription

    checkBuildType (UnknownBuildType name) =
      die $ "The build-type '" ++ name ++ "' is not known. Use one of: "
         ++ intercalate ", " (map display knownBuildTypes) ++ "."
    checkBuildType _ = return ()

-- | Decide if we're going to be able to do a direct internal call to the
-- entry point in libfaction or if we're going to have to compile
-- and execute an external Setup.hs script.
--
determineSetupMethod :: SetupScriptOptions -> BuildType -> SetupMethod
determineSetupMethod options buildType'
  | isJust (useLoggingHandle options)
 || buildType' == Custom      = externalSetupMethod
  | factionVersion `withinRange`
      useFactionVersion options = internalSetupMethod
  | otherwise                 = externalSetupMethod

type SetupMethod = Verbosity
                -> SetupScriptOptions
                -> PackageIdentifier
                -> BuildType
                -> (Version -> [String]) -> IO ()

-- ------------------------------------------------------------
-- * Internal SetupMethod
-- ------------------------------------------------------------

internalSetupMethod :: SetupMethod
internalSetupMethod verbosity options _ bt mkargs = do
  let args = mkargs factionVersion
  debug verbosity $ "Using internal setup method with build-type " ++ show bt
                 ++ " and args:\n  " ++ show args
  inDir (useWorkingDir options) $
    buildTypeAction bt args

buildTypeAction :: BuildType -> ([String] -> IO ())
buildTypeAction Simple    = Simple.defaultMainArgs
buildTypeAction Configure = Simple.defaultMainWithHooksArgs
                              Simple.autoconfUserHooks
buildTypeAction Custom               = error "buildTypeAction Custom"
buildTypeAction (UnknownBuildType _) = error "buildTypeAction UnknownBuildType"

-- ------------------------------------------------------------
-- * External SetupMethod
-- ------------------------------------------------------------

externalSetupMethod :: SetupMethod
externalSetupMethod verbosity options pkg bt mkargs = do
  debug verbosity $ "Using external setup method with build-type " ++ show bt
  createDirectoryIfMissingVerbose verbosity True setupDir
  (libfactionVersion, options') <- libfactionVersionToUse
  debug verbosity $ "Using libfaction version " ++ display libfactionVersion
  setupHs <- updateSetupScript libfactionVersion bt
  debug verbosity $ "Using " ++ setupHs ++ " as setup script."
  compileSetupExecutable options' libfactionVersion setupHs
  invokeSetupScript (mkargs libfactionVersion)

  where
  workingDir       = case fromMaybe "" (useWorkingDir options) of
                       []  -> "."
                       dir -> dir
  setupDir         = workingDir </> useDistPref options </> "setup"
  setupVersionFile = setupDir </> "setup" <.> "version"
  setupProgFile    = setupDir </> "setup" <.> exeExtension

  libfactionVersionToUse :: IO (Version, SetupScriptOptions)
  libfactionVersionToUse = do
    savedVersion <- savedFactionVersion
    case savedVersion of
      Just version | version `withinRange` useFactionVersion options
        -> return (version, options)
      _ -> do (comp, conf, options') <- configureCompiler options
              version <- installedFactionVersion options comp conf
              writeFile setupVersionFile (show version ++ "\n")
              return (version, options')

  savedFactionVersion = do
    versionString <- readFile setupVersionFile `catch` \_ -> return ""
    case reads versionString of
      [(version,s)] | all isSpace s -> return (Just version)
      _                             -> return Nothing

  installedFactionVersion :: SetupScriptOptions -> Compiler
                        -> ProgramConfiguration -> IO Version
  installedFactionVersion _ _ _ | packageName pkg == PackageName "libfaction" =
    return (packageVersion pkg)
  installedFactionVersion options' comp conf = do
    index <- case usePackageIndex options' of
      Just index -> return index
      Nothing    -> getInstalledPackages verbosity
                      comp (usePackageDB options') conf

    let factionDep = Dependency (PackageName "libfaction")
                                (useFactionVersion options)
    case PackageIndex.lookupDependency index factionDep of
      []   -> die $ "The package requires libfaction version "
                 ++ display (useFactionVersion options)
                 ++ " but no suitable version is installed."
      pkgs -> return $ bestVersion (map fst pkgs)
    where
      bestVersion          = maximumBy (comparing preference)
      preference version   = (sameVersion, sameMajorVersion
                             ,stableVersion, latestVersion)
        where
          sameVersion      = version == factionVersion
          sameMajorVersion = majorVersion version == majorVersion factionVersion
          majorVersion     = take 2 . versionBranch
          stableVersion    = case versionBranch version of
                               (_:x:_) -> even x
                               _       -> False
          latestVersion    = version

  configureCompiler :: SetupScriptOptions
                    -> IO (Compiler, ProgramConfiguration, SetupScriptOptions)
  configureCompiler options' = do
    (comp, conf) <- case useCompiler options' of
      Just comp -> return (comp, useProgramConfig options')
      Nothing   -> configCompiler (Just GHC) Nothing Nothing
                     (useProgramConfig options') verbosity
    return (comp, conf, options' { useCompiler = Just comp,
                                   useProgramConfig = conf })

  -- | Decide which Setup.hs script to use, creating it if necessary.
  --
  updateSetupScript :: Version -> BuildType -> IO FilePath
  updateSetupScript _ Custom = do
    useHs  <- doesFileExist setupHs
    useLhs <- doesFileExist setupLhs
    unless (useHs || useLhs) $ die
      "Using 'build-type: Custom' but there is no Setup.hs or Setup.lhs script."
    return (if useHs then setupHs else setupLhs)
    where
      setupHs  = workingDir </> "Setup.hs"
      setupLhs = workingDir </> "Setup.lhs"

  updateSetupScript libfactionVersion _ = do
    rewriteFile setupHs (buildTypeScript libfactionVersion)
    return setupHs
    where
      setupHs  = setupDir </> "setup.hs"

  buildTypeScript :: Version -> String
  buildTypeScript libfactionVersion = case bt of
    Simple    -> "import Distribution.Simple; main = defaultMain\n"
    Configure -> "import Distribution.Simple; main = defaultMainWithHooks "
              ++ if libfactionVersion >= Version [1,3,10] []
                   then "autoconfUserHooks\n"
                   else "defaultUserHooks\n"
    Custom             -> error "buildTypeScript Custom"
    UnknownBuildType _ -> error "buildTypeScript UnknownBuildType"

  -- | If the Setup.hs is out of date wrt the executable then recompile it.
  -- Currently this is GHC only. It should really be generalised.
  --
  compileSetupExecutable :: SetupScriptOptions -> Version -> FilePath -> IO ()
  compileSetupExecutable options' libfactionVersion setupHsFile = do
    setupHsNewer      <- setupHsFile      `moreRecentFile` setupProgFile
    cabalVersionNewer <- setupVersionFile `moreRecentFile` setupProgFile
    let outOfDate = setupHsNewer || cabalVersionNewer
    when outOfDate $ do
      debug verbosity "Setup script is out of date, compiling..."
      (_, conf, _) <- configureCompiler options'
      --TODO: get Faction's GHC module to export a GhcOptions type and render func
      rawSystemProgramConf verbosity ghcProgram conf $
          ghcVerbosityOptions verbosity
       ++ ["--make", setupHsFile, "-o", setupProgFile
          ,"-odir", setupDir, "-hidir", setupDir
          ,"-i", "-i" ++ workingDir ]
       ++ ghcPackageDbOptions (usePackageDB options')
       ++ if packageName pkg == PackageName "libfaction"
            then []
            else ["-package", display libfactionPkgid]
    where
      libfactionPkgid =
        PackageIdentifier (PackageName "libfaction") libfactionVersion

      ghcPackageDbOptions :: PackageDBStack -> [String]
      ghcPackageDbOptions dbstack = case dbstack of
        (GlobalPackageDB:UserPackageDB:dbs) -> concatMap specific dbs
        (GlobalPackageDB:dbs)               -> "-no-user-package-conf"
                                             : concatMap specific dbs
        _                                   -> ierror
        where
          specific (SpecificPackageDB db) = [ "-package-conf", db ]
          specific _ = ierror
          ierror     = error "internal error: unexpected package db stack"


  invokeSetupScript :: [String] -> IO ()
  invokeSetupScript args = do
    info verbosity $ unwords (setupProgFile : args)
    case useLoggingHandle options of
      Nothing        -> return ()
      Just logHandle -> info verbosity $ "Redirecting build log to "
                                      ++ show logHandle
    currentDir <- getCurrentDirectory
    process <- runProcess (currentDir </> setupProgFile) args
                 (useWorkingDir options) Nothing
                 Nothing (useLoggingHandle options) (useLoggingHandle options)
    exitCode <- waitForProcess process
    unless (exitCode == ExitSuccess) $ exitWith exitCode
