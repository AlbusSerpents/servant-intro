{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_servent_intro (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/serpents/.cabal/bin"
libdir     = "/home/serpents/.cabal/lib/x86_64-linux-ghc-8.6.3/servent-intro-0.1.0.0-H0Pcjoko5nW7kCIf02pkmD"
dynlibdir  = "/home/serpents/.cabal/lib/x86_64-linux-ghc-8.6.3"
datadir    = "/home/serpents/.cabal/share/x86_64-linux-ghc-8.6.3/servent-intro-0.1.0.0"
libexecdir = "/home/serpents/.cabal/libexec"
sysconfdir = "/home/serpents/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "servent_intro_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "servent_intro_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "servent_intro_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "servent_intro_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "servent_intro_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "servent_intro_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
