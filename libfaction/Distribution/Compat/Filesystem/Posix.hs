module Distribution.Compat.Filesystem.Posix
  (pathSeparator,
   pathSeparators,
   pathCoerceToDirectory,
   pathCoerceToFile,
   pathIsAbsolute,
   pathIsRelative,
   pathIsDirectory,
   pathIsFile,
   (</>),
   removeFileVerbose,
   removeDirectoryVerbose,
   removeDirectoryRecursiveVerbose,
   listDirectory)
  where

import Control.Exception
import Data.List
import Distribution.Compat.Filesystem.Portable
import Distribution.Simple.Utils
import Distribution.Verbosity
import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix


pathSeparator :: Char
pathSeparator = '/'


pathSeparators :: [Char]
pathSeparators = ['/']


removeFileVerbose :: Verbosity -> FilePath -> IO ()
removeFileVerbose verbosity path = do
  debug verbosity $ "Removing file " ++ path
  Posix.removeLink $ pathCoerceToFile path


removeDirectoryVerbose :: Verbosity -> FilePath -> IO ()
removeDirectoryVerbose verbosity path = do
  debug verbosity $ "Removing directory " ++ path
  Posix.removeDirectory $ pathCoerceToFile path


removeDirectoryRecursiveVerbose :: Verbosity -> FilePath -> IO ()
removeDirectoryRecursiveVerbose verbosity path = do
  debug verbosity $ "Removing directory recursively " ++ path
  let visit path = do
        exists <- Posix.fileExist path
        if exists
          then do
            fileStatus <- Posix.getSymbolicLinkStatus path
            if Posix.isSymbolicLink fileStatus
              then visitFile path
              else do
                if Posix.isDirectory fileStatus
                  then visitDirectory path
                  else visitFile path
          else return ()
      visitFile path = do
        removeFileVerbose verbosity path
      visitDirectory path = do
        itemPaths <- listDirectory path
        mapM_ visit
              $ map (path </>) $ itemPaths \\ [".", ".."]
        removeDirectoryVerbose verbosity path
  visit path


listDirectory :: FilePath -> IO [FilePath]
listDirectory path = do
  bracket (Posix.openDirStream path)
          (Posix.closeDirStream)
          (\dirStream -> do
             let loop accumulator = do
                   itemPath <- Posix.readDirStream dirStream
                   if itemPath == ""
                     then return accumulator
                     else loop $ accumulator ++ [itemPath]
             loop [])
