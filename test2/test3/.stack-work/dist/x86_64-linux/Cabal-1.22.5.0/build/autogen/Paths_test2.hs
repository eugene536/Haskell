module Paths_test2 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/eugene/university/4course/haskell/test2/test2/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/bin"
libdir     = "/home/eugene/university/4course/haskell/test2/test2/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/lib/x86_64-linux-ghc-7.10.3/test2-0.1.0.0-JQULrSP5qhp1sMjou4dkJT"
datadir    = "/home/eugene/university/4course/haskell/test2/test2/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/share/x86_64-linux-ghc-7.10.3/test2-0.1.0.0"
libexecdir = "/home/eugene/university/4course/haskell/test2/test2/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/libexec"
sysconfdir = "/home/eugene/university/4course/haskell/test2/test2/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "test2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "test2_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "test2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "test2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "test2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
