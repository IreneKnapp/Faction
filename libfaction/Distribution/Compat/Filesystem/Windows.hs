module Distribution.Compat.Filesystem.Windows
  (pathSeparator,
   pathSeparators,
   removeFileVerbose,
   removeDirectoryVerbose,
   removeDirectoryRecursiveVerbose,
   listDirectory)
  where

import Distribution.Verbosity


pathSeparator :: Char
pathSeparator = '\\'


pathSeparators :: [Char]
pathSeparators = ['\\', '/']


removeFileVerbose :: Verbosity -> FilePath -> IO ()
removeFileVerbose verbosity path = do
  error "Unimplemented."


removeDirectoryVerbose :: Verbosity -> FilePath -> IO ()
removeDirectoryVerbose verbosity path = do
  error "Unimplemented."


removeDirectoryRecursiveVerbose :: Verbosity -> FilePath -> IO ()
removeDirectoryRecursiveVerbose verbosity path = do
  error "Unimplemented."


listDirectory :: FilePath -> IO [FilePath]
listDirectory path = do
  error "Unimplemented."
