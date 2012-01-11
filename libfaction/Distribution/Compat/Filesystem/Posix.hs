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
   removeFileSilently,
   removeDirectorySilently,
   removeDirectoryRecursiveSilently,
   listDirectory)
  where

import Control.Exception
import Data.List
import Distribution.Compat.Filesystem.Portable
import qualified System.Posix.Directory as Posix
import qualified System.Posix.Files as Posix


pathSeparator :: Char
pathSeparator = '/'


pathSeparators :: [Char]
pathSeparators = ['/']


removeFileSilently :: FilePath -> IO ()
removeFileSilently path = do
  Posix.removeLink $ pathCoerceToFile path


removeDirectorySilently :: FilePath -> IO ()
removeDirectorySilently path = do
  Posix.removeLink $ pathCoerceToFile path


removeDirectoryRecursiveSilently :: FilePath -> IO ()
removeDirectoryRecursiveSilently path = do
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
        removeFileSilently path
      visitDirectory path = do
        itemPaths <- listDirectory path
        mapM_ visit
              $ map (path </>) $ itemPaths \\ [".", ".."]
        removeDirectorySilently path
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
