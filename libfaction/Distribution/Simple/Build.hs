{-
This is the entry point to actually building the modules in a package. It
doesn't actually do much itself, most of the work is delegated to
compiler-specific actions. It does do some non-compiler specific bits like
running pre-processors.
-}

module Distribution.Simple.Build (
    build,

    initialBuildSteps,
    writeAutogenFiles,
  ) where

import qualified Distribution.Simple.GHC  as GHC

import qualified Distribution.Simple.Build.Macros      as Build.Macros
import qualified Distribution.Simple.Build.PathsModule as Build.PathsModule

import Distribution.Package
         ( Package(..), PackageName(..), PackageIdentifier(..)
         , Dependency(..), thisPackageVersion )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), compilerFlavor, PackageDB(..) )
import Distribution.PackageDescription
         ( PackageDescription(..), BuildInfo(..), Library(..), Executable(..)
         , App(..), TestSuite(..), TestSuiteInterface(..), Benchmark(..)
         , BenchmarkInterface(..) )
import qualified Distribution.InstalledPackageInfo as IPI
import qualified Distribution.ModuleName as ModuleName

import Distribution.Simple.Setup
         ( BuildFlags(..), fromFlag )
import Distribution.Simple.PreProcess
         ( preprocessComponent, PPSuffixHandler )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(compiler, buildDir, withPackageDB, withPrograms)
         , Component(..), ComponentLocalBuildInfo(..), withComponentsLBI
         , inplacePackageId )
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db
import Distribution.Simple.BuildPaths
         ( autogenModulesDir, autogenModuleName, cppHeaderName, exeExtension )
import Distribution.Simple.Register
         ( registerPackage, inplaceInstalledPackageInfo )
import Distribution.Simple.Test ( stubFilePath, stubName )
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose, rewriteFile
         , die, info, setupMessage )

import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text
         ( display )

import Data.Maybe
         ( maybeToList )
import Data.List
         ( intersect )
import Control.Monad
         ( unless )
import System.FilePath
         ( (</>), (<.>) )
import System.Directory
         ( getCurrentDirectory )

-- -----------------------------------------------------------------------------
-- |Build the libraries, executables, and apps in this package.

build    :: PackageDescription  -- ^ Mostly information from the .cabal file
         -> LocalBuildInfo      -- ^ Configuration information
         -> BuildFlags          -- ^ Flags that the user passed to build
         -> [ PPSuffixHandler ] -- ^ preprocessors to run before compiling
         -> IO ()
build pkg_descr lbi flags suffixes = do
  let distPref  = fromFlag (buildDistPref flags)
      verbosity = fromFlag (buildVerbosity flags)
  initialBuildSteps distPref pkg_descr lbi verbosity
  setupMessage verbosity "Building" (packageId pkg_descr)

  internalPackageDB <- createInternalPackageDB distPref

  let pre c lbi' = preprocessComponent pkg_descr c lbi' False verbosity suffixes
  withComponentsLBI pkg_descr lbi $ \comp clbi ->
    case comp of
      CLib lib -> do
        let bi     = libBuildInfo lib
            progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
            lbi'   = lbi { withPrograms = progs' }
        pre comp lbi'
        info verbosity "Building library..."
        buildLib verbosity pkg_descr lbi' lib clbi

        -- Register the library in-place, so exes can depend
        -- on internally defined libraries.
        pwd <- getCurrentDirectory
        let installedPkgInfo =
              (inplaceInstalledPackageInfo pwd distPref pkg_descr lib lbi clbi) {
                -- The inplace registration uses the "-inplace" suffix,
                -- not an ABI hash.
                IPI.installedPackageId = inplacePackageId (packageId installedPkgInfo)
              }
        registerPackage verbosity
          installedPkgInfo pkg_descr lbi True -- True meaning inplace
          (withPackageDB lbi ++ [internalPackageDB])

      CExe exe -> do
        let bi     = buildInfo exe
            progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
            lbi'   = lbi {
                       withPrograms  = progs',
                       withPackageDB = withPackageDB lbi ++ [internalPackageDB]
                     }
        pre comp lbi'
        info verbosity $ "Building executable " ++ exeName exe ++ "..."
        buildExe verbosity pkg_descr lbi' exe clbi

      CApp app -> do
        let bi     = appBuildInfo app
            progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
            lbi'   = lbi {
                       withPrograms  = progs',
                       withPackageDB = withPackageDB lbi ++ [internalPackageDB]
                     }
        pre comp lbi'
        info verbosity $ "Building app " ++ appName app ++ "..."
        buildApp verbosity pkg_descr lbi' app clbi

      CTest test -> do
        case testInterface test of
            TestSuiteExeV10 _ f -> do
                let bi  = testBuildInfo test
                    exe = Executable
                        { exeName = testName test
                        , modulePath = f
                        , buildInfo  = bi
                        }
                    progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
                    lbi'   = lbi {
                               withPrograms  = progs',
                               withPackageDB = withPackageDB lbi ++ [internalPackageDB]
                             }
                pre comp lbi'
                info verbosity $ "Building test suite " ++ testName test ++ "..."
                buildExe verbosity pkg_descr lbi' exe clbi
            TestSuiteLibV09 _ m -> do
                pwd <- getCurrentDirectory
                let bi  = testBuildInfo test
                    lib = Library
                        { exposedModules = [ m ]
                        , libExposed = True
                        , libBuildInfo = bi
                        }
                    pkg = pkg_descr
                        { package = (package pkg_descr)
                            { pkgName = PackageName $ testName test
                            }
                        , buildDepends = targetBuildDepends $ testBuildInfo test
                        , executables = []
                        , testSuites = []
                        , library = Just lib
                        }
                    ipi = (inplaceInstalledPackageInfo
                        pwd distPref pkg lib lbi clbi)
                        { IPI.installedPackageId = inplacePackageId $ packageId ipi
                        }
                    testDir = buildDir lbi' </> stubName test
                        </> stubName test ++ "-tmp"
                    testLibDep = thisPackageVersion $ package pkg
                    exe = Executable
                        { exeName = stubName test
                        , modulePath = stubFilePath test
                        , buildInfo = (testBuildInfo test)
                            { hsSourceDirs = [ testDir ]
                            , targetBuildDepends = testLibDep
                                : (targetBuildDepends $ testBuildInfo test)
                            }
                        }
                    -- | The stub executable needs a new 'ComponentLocalBuildInfo'
                    -- that exposes the relevant test suite library.
                    exeClbi = clbi
                        { componentPackageDeps =
                            (IPI.installedPackageId ipi, packageId ipi)
                            : (filter (\(_, x) -> let PackageName name = pkgName x in name == "Cabal" || name == "base")
                                $ componentPackageDeps clbi)
                        }
                    progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
                    lbi'   = lbi {
                               withPrograms  = progs',
                               withPackageDB = withPackageDB lbi ++ [internalPackageDB]
                             }

                pre comp lbi'
                info verbosity $ "Building test suite " ++ testName test ++ "..."
                buildLib verbosity pkg lbi' lib clbi
                registerPackage verbosity ipi pkg lbi' True $ withPackageDB lbi'
                buildExe verbosity pkg_descr lbi' exe exeClbi
            TestSuiteUnsupported tt -> die $ "No support for building test suite "
                                          ++ "type " ++ display tt

      CBench bm -> do
        case benchmarkInterface bm of
            BenchmarkExeV10 _ f -> do
                let bi  = benchmarkBuildInfo bm
                    exe = Executable
                        { exeName = benchmarkName bm
                        , modulePath = f
                        , buildInfo  = bi
                        }
                    progs' = addInternalBuildTools pkg_descr lbi bi (withPrograms lbi)
                    lbi'   = lbi {
                               withPrograms  = progs',
                               withPackageDB = withPackageDB lbi ++ [internalPackageDB]
                             }
                pre comp lbi'
                info verbosity $ "Building benchmark " ++ benchmarkName bm ++ "..."
                buildExe verbosity pkg_descr lbi' exe clbi
            BenchmarkUnsupported tt -> die $ "No support for building benchmark "
                                          ++ "type " ++ display tt

-- | Initialize a new package db file for libraries defined
-- internally to the package.
createInternalPackageDB :: FilePath -> IO PackageDB
createInternalPackageDB distPref = do
    let dbFile = distPref </> "package.conf.inplace"
        packageDB = SpecificPackageDB dbFile
    writeFile dbFile "[]"
    return packageDB

addInternalBuildTools :: PackageDescription -> LocalBuildInfo -> BuildInfo
                      -> ProgramDb -> ProgramDb
addInternalBuildTools pkg lbi bi progs =
    foldr updateProgram progs internalBuildTools
  where
    internalBuildTools =
      [ simpleConfiguredProgram toolName (FoundOnSystem toolLocation)
      | toolName <- toolNames
      , let toolLocation = buildDir lbi </> toolName </> toolName <.> exeExtension ]
    toolNames = intersect buildToolNames internalExeNames
    internalExeNames = map exeName (executables pkg)
    buildToolNames   = map buildToolName (buildTools bi)
      where
        buildToolName (Dependency (PackageName name) _ ) = name


-- TODO: build separate libs in separate dirs so that we can build
-- multiple libs, e.g. for 'LibTest' library-style testsuites
buildLib :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib verbosity pkg_descr lbi lib clbi =
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.buildLib  verbosity pkg_descr lbi lib clbi
    _    -> die "Building is not supported with this compiler."

buildExe :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe verbosity pkg_descr lbi exe clbi =
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.buildExe  verbosity pkg_descr lbi exe clbi
    _    -> die "Building is not supported with this compiler."

buildApp :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> App                -> ComponentLocalBuildInfo -> IO ()
buildApp verbosity pkg_descr lbi app clbi =
  case compilerFlavor (compiler lbi) of
    GHC  -> GHC.buildApp  verbosity pkg_descr lbi app clbi
    _    -> die "Building is not supported with this compiler."

initialBuildSteps :: FilePath -- ^"dist" prefix
                  -> PackageDescription  -- ^mostly information from the .faction file
                  -> LocalBuildInfo -- ^Configuration information
                  -> Verbosity -- ^The verbosity to use
                  -> IO ()
initialBuildSteps _distPref pkg_descr lbi verbosity = do
  -- check that there's something to build
  let buildInfos =
          map libBuildInfo (maybeToList (library pkg_descr)) ++
          map buildInfo (executables pkg_descr) ++
          map appBuildInfo (apps pkg_descr)
  unless (any buildable buildInfos) $ do
    let name = display (packageId pkg_descr)
    die ("Package " ++ name ++ " can't be built on this system.")

  createDirectoryIfMissingVerbose verbosity True (buildDir lbi)

  writeAutogenFiles verbosity pkg_descr lbi

-- | Generate and write out the Paths_<pkg>.hs and faction_macros.h files
--
writeAutogenFiles :: Verbosity
                  -> PackageDescription
                  -> LocalBuildInfo
                  -> IO ()
writeAutogenFiles verbosity pkg lbi = do
  createDirectoryIfMissingVerbose verbosity True (autogenModulesDir lbi)

  let pathsModulePath = autogenModulesDir lbi
                    </> ModuleName.toFilePath (autogenModuleName pkg) <.> "hs"
  rewriteFile pathsModulePath (Build.PathsModule.generate pkg lbi)

  let cppHeaderPath = autogenModulesDir lbi </> cppHeaderName
  rewriteFile cppHeaderPath (Build.Macros.generate pkg lbi)
