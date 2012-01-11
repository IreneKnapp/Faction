{-# LANGUAGE CPP #-}
module Distribution.Compat.Filesystem.Portable
  (pathCoerceToDirectory,
   pathCoerceToFile,
   pathIsAbsolute,
   pathIsRelative,
   pathIsDirectory,
   pathIsFile,
   (</>))
  where

#ifdef VERSION_unix
import {-# SOURCE #-} Distribution.Compat.Filesystem.Posix
#else
import Distribution.Compat.Filesystem.Windows
#endif


pathCoerceToDirectory :: FilePath -> FilePath
pathCoerceToDirectory "" = ['.', pathSeparator]
pathCoerceToDirectory path =
  if elem (last path) pathSeparators
    then path
    else path ++ [pathSeparator]


pathCoerceToFile :: FilePath -> FilePath
pathCoerceToFile "" = "."
pathCoerceToFile path =
  if pathIsDirectory path
    then init path
    else path


pathIsAbsolute :: FilePath -> Bool
pathIsAbsolute "" = False
pathIsAbsolute (c:_) = elem c pathSeparators


pathIsRelative :: FilePath -> Bool
pathIsRelative = not . pathIsAbsolute


pathIsDirectory :: FilePath -> Bool
pathIsDirectory "" = True
pathIsDirectory path = elem (last path) pathSeparators


pathIsFile :: FilePath -> Bool
pathIsFile = not . pathIsDirectory


(</>) :: FilePath -> FilePath -> FilePath
parent </> child =
  if pathIsRelative child
    then pathCoerceToDirectory parent ++ child
    else child
