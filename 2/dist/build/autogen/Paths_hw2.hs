{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_hw2 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/u/mspohrer/.cabal/bin"
libdir     = "/u/mspohrer/.cabal/lib/x86_64-linux-ghc-8.0.2/hw2-1.1.0.0-KneR2doEYZ86jowjQXiFCx"
dynlibdir  = "/u/mspohrer/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/u/mspohrer/.cabal/share/x86_64-linux-ghc-8.0.2/hw2-1.1.0.0"
libexecdir = "/u/mspohrer/.cabal/libexec"
sysconfdir = "/u/mspohrer/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hw2_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hw2_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hw2_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hw2_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw2_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
