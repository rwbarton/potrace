module Graphics.Potrace (
  potraceVersion
  ) where

import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (peekCString)

import qualified Bindings.Potrace as B

potraceVersion :: String
potraceVersion = unsafePerformIO $ peekCString =<< B.c'potrace_version
