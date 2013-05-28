module Paths_libfaction (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/libfaction-1.0/ghc-7.6.2"
datadir    = "/usr/local/share/libfaction-1.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "libfaction_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "libfaction_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "libfaction_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "libfaction_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
