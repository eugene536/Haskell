module Paths_ParserHw (
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

bindir     = "/home/eugene/university/4course/haskell/univer/hw/05_applicative/ParserHw/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/bin"
libdir     = "/home/eugene/university/4course/haskell/univer/hw/05_applicative/ParserHw/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/lib/x86_64-linux-ghc-7.10.3/ParserHw-0.1.0.0-8jvQSOrf1Ae9tCV8prCCeR"
datadir    = "/home/eugene/university/4course/haskell/univer/hw/05_applicative/ParserHw/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/share/x86_64-linux-ghc-7.10.3/ParserHw-0.1.0.0"
libexecdir = "/home/eugene/university/4course/haskell/univer/hw/05_applicative/ParserHw/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/libexec"
sysconfdir = "/home/eugene/university/4course/haskell/univer/hw/05_applicative/ParserHw/.stack-work/install/x86_64-linux/lts-6.23/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ParserHw_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ParserHw_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ParserHw_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ParserHw_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ParserHw_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
