module Distribution.Compat.Filesystem.Windows
  (pathSeparator,
   pathSeparators,
   removeFileSilently,
   removeDirectorySilently,
   removeDirectoryRecursiveSilently,
   listDirectory)
  where


pathSeparator :: Char
pathSeparator = '\\'


pathSeparators :: [Char]
pathSeparators = ['\\', '/']


removeFileSilently :: FilePath -> IO ()
removeFileSilently path = do
  error "Unimplemented."


removeDirectorySilently :: FilePath -> IO ()
removeDirectorySilently path = do
  error "Unimplemented."


removeDirectoryRecursiveSilently :: FilePath -> IO ()
removeDirectoryRecursiveSilently path = do
  error "Unimplemented."


listDirectory :: FilePath -> IO [FilePath]
listDirectory path = do
  error "Unimplemented."
