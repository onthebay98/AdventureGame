{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_functional_adventure (
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
bindir     = "/Users/bayhodge/Desktop/AdventureGame/.stack-work/install/aarch64-osx/19d473ede00886b8815e0ea5e45a6e0eebe51264c00f4c122beeb782d452da95/9.4.6/bin"
libdir     = "/Users/bayhodge/Desktop/AdventureGame/.stack-work/install/aarch64-osx/19d473ede00886b8815e0ea5e45a6e0eebe51264c00f4c122beeb782d452da95/9.4.6/lib/aarch64-osx-ghc-9.4.6/functional-adventure-0.1.0.0-EbTHqK6fUbl47JCfTG01M1"
dynlibdir  = "/Users/bayhodge/Desktop/AdventureGame/.stack-work/install/aarch64-osx/19d473ede00886b8815e0ea5e45a6e0eebe51264c00f4c122beeb782d452da95/9.4.6/lib/aarch64-osx-ghc-9.4.6"
datadir    = "/Users/bayhodge/Desktop/AdventureGame/.stack-work/install/aarch64-osx/19d473ede00886b8815e0ea5e45a6e0eebe51264c00f4c122beeb782d452da95/9.4.6/share/aarch64-osx-ghc-9.4.6/functional-adventure-0.1.0.0"
libexecdir = "/Users/bayhodge/Desktop/AdventureGame/.stack-work/install/aarch64-osx/19d473ede00886b8815e0ea5e45a6e0eebe51264c00f4c122beeb782d452da95/9.4.6/libexec/aarch64-osx-ghc-9.4.6/functional-adventure-0.1.0.0"
sysconfdir = "/Users/bayhodge/Desktop/AdventureGame/.stack-work/install/aarch64-osx/19d473ede00886b8815e0ea5e45a6e0eebe51264c00f4c122beeb782d452da95/9.4.6/etc"

getBinDir     = catchIO (getEnv "functional_adventure_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "functional_adventure_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "functional_adventure_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "functional_adventure_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "functional_adventure_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "functional_adventure_sysconfdir") (\_ -> return sysconfdir)




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
