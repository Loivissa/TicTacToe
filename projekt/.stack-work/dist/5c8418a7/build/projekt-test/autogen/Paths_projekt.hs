{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_projekt (
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

bindir     = "D:\\WIEiT\\Funkcyjne\\TicTacToe\\projekt\\.stack-work\\install\\ccbce92a\\bin"
libdir     = "D:\\WIEiT\\Funkcyjne\\TicTacToe\\projekt\\.stack-work\\install\\ccbce92a\\lib\\x86_64-windows-ghc-8.2.2\\projekt-0.1.0.0-3xI1GD1Il5nIyv9D87MeYB-projekt-test"
dynlibdir  = "D:\\WIEiT\\Funkcyjne\\TicTacToe\\projekt\\.stack-work\\install\\ccbce92a\\lib\\x86_64-windows-ghc-8.2.2"
datadir    = "D:\\WIEiT\\Funkcyjne\\TicTacToe\\projekt\\.stack-work\\install\\ccbce92a\\share\\x86_64-windows-ghc-8.2.2\\projekt-0.1.0.0"
libexecdir = "D:\\WIEiT\\Funkcyjne\\TicTacToe\\projekt\\.stack-work\\install\\ccbce92a\\libexec\\x86_64-windows-ghc-8.2.2\\projekt-0.1.0.0"
sysconfdir = "D:\\WIEiT\\Funkcyjne\\TicTacToe\\projekt\\.stack-work\\install\\ccbce92a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "projekt_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "projekt_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "projekt_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "projekt_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "projekt_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "projekt_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
