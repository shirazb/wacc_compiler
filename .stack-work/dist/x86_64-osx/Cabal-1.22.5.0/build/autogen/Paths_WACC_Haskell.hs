module Paths_WACC_Haskell (
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

bindir     = "/Users/sarahbaka/Desktop/WACC-Haskell/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/bin"
libdir     = "/Users/sarahbaka/Desktop/WACC-Haskell/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/lib/x86_64-osx-ghc-7.10.3/WACC-Haskell-0.1.0.0-4D8jym07S8F37B90YtUWJJ"
datadir    = "/Users/sarahbaka/Desktop/WACC-Haskell/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/share/x86_64-osx-ghc-7.10.3/WACC-Haskell-0.1.0.0"
libexecdir = "/Users/sarahbaka/Desktop/WACC-Haskell/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/libexec"
sysconfdir = "/Users/sarahbaka/Desktop/WACC-Haskell/.stack-work/install/x86_64-osx/lts-6.11/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "WACC_Haskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "WACC_Haskell_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "WACC_Haskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "WACC_Haskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "WACC_Haskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
