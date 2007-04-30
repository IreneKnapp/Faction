{-# OPTIONS -cpp #-}
-- #hide
module Distribution.Compat.TempFile (openTempFile) where

import System.IO (openFile, Handle, IOMode(ReadWriteMode))
import System.Directory (doesFileExist)

import Distribution.Compat.FilePath (joinFileName)

#if (__GLASGOW_HASKELL__ || __HUGS__) && !(mingw32_HOST_OS || mingw32_TARGET_OS)
import System.Posix.Internals (c_getpid)
#else
import System.Posix.Types (CPid(..))
#endif


-- ------------------------------------------------------------
-- * temporary files
-- ------------------------------------------------------------

-- TODO: this function *really really really* should be
--       eliminated and replaced with System.IO.openTempFile,
--       except that is currently GHC-only for no valid reason.

-- use a temporary filename that doesn't already exist.
-- NB. *not* secure (we don't atomically lock the tmp file we get)
openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile tmp_dir template
  = do x <- getProcessID
       findTempName x
  where 
    findTempName x
      = do let filename = template ++ show x
	       path = tmp_dir `joinFileName` filename
  	   b  <- doesFileExist path
	   if b then findTempName (x+1)
		else do hnd <- openFile path ReadWriteMode
                        return (path, hnd)

#if mingw32_HOST_OS || mingw32_TARGET_OS
foreign import ccall unsafe "_getpid" getProcessID :: IO Int
		 -- XXX relies on Int == Int32 on Windows
#else
#if !(__GLASGOW_HASKELL__ || __HUGS__)
foreign import ccall unsafe "getpid" c_getpid :: IO CPid
#endif
getProcessID :: IO Int
getProcessID = c_getpid >>= return . fromIntegral
#endif
