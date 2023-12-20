{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hw67 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/dev.daniil.bakushkin/.cabal/bin"
libdir     = "/Users/dev.daniil.bakushkin/.cabal/lib/aarch64-osx-ghc-9.4.7/hw67-0.1.0.0-inplace"
dynlibdir  = "/Users/dev.daniil.bakushkin/.cabal/lib/aarch64-osx-ghc-9.4.7"
datadir    = "/Users/dev.daniil.bakushkin/.cabal/share/aarch64-osx-ghc-9.4.7/hw67-0.1.0.0"
libexecdir = "/Users/dev.daniil.bakushkin/.cabal/libexec/aarch64-osx-ghc-9.4.7/hw67-0.1.0.0"
sysconfdir = "/Users/dev.daniil.bakushkin/.cabal/etc"

getBinDir     = catchIO (getEnv "hw67_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hw67_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hw67_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hw67_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hw67_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hw67_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
