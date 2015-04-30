module Paths_Sokal (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/kiraghandhi/Documents/3rd Year/SpringQuarter/Haskell/labs/cmsc-22311/Lab1-Sokal/.cabal-sandbox/bin"
libdir     = "/Users/kiraghandhi/Documents/3rd Year/SpringQuarter/Haskell/labs/cmsc-22311/Lab1-Sokal/.cabal-sandbox/lib/x86_64-osx-ghc-7.8.3/Sokal-0.1.0.0"
datadir    = "/Users/kiraghandhi/Documents/3rd Year/SpringQuarter/Haskell/labs/cmsc-22311/Lab1-Sokal/.cabal-sandbox/share/x86_64-osx-ghc-7.8.3/Sokal-0.1.0.0"
libexecdir = "/Users/kiraghandhi/Documents/3rd Year/SpringQuarter/Haskell/labs/cmsc-22311/Lab1-Sokal/.cabal-sandbox/libexec"
sysconfdir = "/Users/kiraghandhi/Documents/3rd Year/SpringQuarter/Haskell/labs/cmsc-22311/Lab1-Sokal/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Sokal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Sokal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Sokal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Sokal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Sokal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
