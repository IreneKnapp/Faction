-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Config
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Utilities for handling saved state such as known packages, known servers and downloaded packages.
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Config
    ( packagesDirectory
    , repoCacheDir
    , packageFile
    , packageDir
    , getDefaultConfigDir
    , getLocalConfigDir
    , getLocalCacheDir
    , getLocalPkgListDir
    , getKnownServers
    , getKnownPackages
    , selectValidConfigDir
    ) where

import Prelude hiding (catch)
import Control.Exception (catch, Exception(IOException),evaluate)
import Control.Monad.Error (mplus, filterM) -- Using Control.Monad.Error to get the Error instance for IO.
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import System.Directory (Permissions (..), getPermissions, createDirectoryIfMissing
	                    ,getTemporaryDirectory)
import System.IO.Error (isDoesNotExistError)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe

import Distribution.Package (PackageIdentifier(..), showPackageId)
import Distribution.PackageDescription (GenericPackageDescription(..)
                                       , PackageDescription(..)
                                       , parsePackageDescription, ParseResult(..))
import Distribution.Version (Dependency, showVersion)
import Distribution.Verbosity
import System.FilePath ((</>), takeExtension, (<.>))
import System.Directory

import Network.Hackage.CabalInstall.Tar (readTarArchive)
import Network.Hackage.CabalInstall.Types (ConfigFlags (..), OutputGen(..), PkgInfo (..), Repo(..))

import Paths_cabal_install (getDataDir)

-- |Compute the global config directory
-- (eg '/usr/local/share/cabal-install-0.3.0/' on Linux).
getDefaultConfigDir :: IO FilePath
getDefaultConfigDir = getDataDir

-- |Compute the local config directory ('~/.cabal-install' on Linux).
getLocalConfigDir :: IO FilePath
getLocalConfigDir
    = getAppUserDataDirectory "cabal-install"

getLocalCacheDir :: IO FilePath
getLocalCacheDir = getLocalConfigDir

getLocalPkgListDir :: IO FilePath
getLocalPkgListDir = getLocalConfigDir

pkgListFile :: FilePath
pkgListFile = "pkg.list"

servListFile :: FilePath
servListFile = "serv.list"

-- |Name of the packages directory.
packagesDirectoryName :: FilePath
packagesDirectoryName = "packages"

-- | Full path to the server list file
servList :: ConfigFlags -> FilePath
servList cfg = configConfDir cfg </> servListFile

-- | Full path to the packages directory.
packagesDirectory :: ConfigFlags -> FilePath
packagesDirectory cfg = configCacheDir cfg </> packagesDirectoryName

-- | Full path to the local cache directory for a repository.
repoCacheDir :: ConfigFlags -> Repo -> FilePath
repoCacheDir cfg repo = packagesDirectory cfg </> repoName repo

-- |Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifer@.
packageFile :: ConfigFlags -> PackageIdentifier -> Repo -> FilePath
packageFile cfg pkg repo = packageDir cfg pkg repo
                           </> showPackageId pkg 
                           <.> "tar.gz"

-- |Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifer@ is stored.
packageDir :: ConfigFlags -> PackageIdentifier -> Repo -> FilePath
packageDir cfg pkg repo = repoCacheDir cfg repo
                      </> pkgName pkg
                      </> showVersion (pkgVersion pkg)

getKnownPackages :: ConfigFlags -> IO [PkgInfo]
getKnownPackages cfg
    = fmap concat $ mapM (readRepoIndex cfg) $ configServers cfg

readRepoIndex :: ConfigFlags -> Repo -> IO [PkgInfo]
readRepoIndex cfg repo =
    do let indexFile = repoCacheDir cfg repo </> "00-index.tar"
       fmap (parseRepoIndex repo) (BS.readFile indexFile)
          `catch` (\e
                 -> do hPutStrLn stderr ("Warning: Problem opening package list '"
                                          ++ indexFile ++ "'.")
                       case e of
                         IOException ioe | isDoesNotExistError ioe ->
                           hPutStrLn stderr "File doesn't exist. Run 'cabal-install update' to create the package list."
                         _ -> hPutStrLn stderr ("Error: " ++ (show e))
                       return [])

parseRepoIndex :: Repo -> ByteString -> [PkgInfo]
parseRepoIndex repo s =
    do (name, content) <- readTarArchive s
       if takeExtension name == ".cabal"
         then case parsePackageDescription (BS.unpack content) of
                    ParseOk _ descr -> return $ PkgInfo { 
                                                         pkgRepo = repo,
                                                         pkgDesc = descr
                                                        }
                    _               -> error $ "Couldn't read cabal file " ++ show name
         else fail "Not a .cabal file"

getKnownServers :: ConfigFlags -> IO [Repo]
getKnownServers cfg
    = (evaluate =<< fmap readRepos (readFile (servList cfg)))
      `catch` \e -> case e of
                         IOException ioe | isDoesNotExistError ioe ->
                           return defaultServs
                         _ -> hPutStrLn stderr ("Failed to read server list: " ++ (show e) ++ ". Using hackage.haskell.org.") >> return defaultServs
          where defaultServs = [ Repo "hackage.haskell.org" "http://hackage.haskell.org/packages/archive" ]
readRepos :: String -> [Repo]
readRepos = map (\ (n,u) -> Repo { repoName = n, repoURL = u }) . read

-- |Confirms validity of a config directory by checking the permissions for the package-list file,
--  server-list file and downloaded packages directory.
isValidConfigDir :: FilePath -> IO Bool
isValidConfigDir path
    = do checks <- sequence
                   [ checkFiles readable [ path ]]
--                                         , path </> servListFile ]]
         return (and checks)

-- |Picks the first valid config directory or throws an exception if none were found.
selectValidConfigDir :: [FilePath] -> IO FilePath
selectValidConfigDir paths
    = do valids <- filterM isValidConfigDir paths
         case valids of
           []    -> error "No valid config dir found!"
           (x:_) -> return x

checkFiles :: (Permissions -> Bool) -> [FilePath] -> IO Bool
checkFiles check
    = worker True
    where worker r [] = return r
          worker r (x:xs)
              = do permissions <- getPermissions x
                   if check permissions
                      then worker r xs
                      else return False
              `mplus` worker False xs