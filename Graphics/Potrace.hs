{-# LANGUAGE Rank2Types #-}

module Graphics.Potrace (
  potraceVersion,
  Options,
  defaultOptions,
  TurnPolicy(..),
  withTurdSize, withTurnPolicy, withAlphaMax, withOptimization
  ) where

import Control.Exception (bracket)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, poke)
import System.IO.Unsafe (unsafePerformIO)

import qualified Bindings.Potrace as B

newtype Options = Options { runOption :: forall r. (Ptr B.C'potrace_param_t -> IO r) -> IO r }

defaultOptions :: Options
defaultOptions = Options $ bracket B.c'potrace_param_default B.c'potrace_param_free

withOption :: Storable a => (Ptr B.C'potrace_param_t -> Ptr a) -> a -> Options -> Options
withOption field val (Options k)
  = Options $ \k' -> k $ \p -> poke (field p) val >> k' p -- XXX implement with Codensity IO monad?

withTurdSize :: Int -> Options -> Options
withTurdSize n = withOption B.p'potrace_param_s'turdsize (fromIntegral n)

data TurnPolicy = TPBlack | TPWhite | TPLeft | TPRight | TPMinority | TPMajority | TPRandom

tpToCInt :: TurnPolicy -> CInt
tpToCInt TPBlack    = B.c'POTRACE_TURNPOLICY_BLACK
tpToCInt TPWhite    = B.c'POTRACE_TURNPOLICY_WHITE
tpToCInt TPLeft     = B.c'POTRACE_TURNPOLICY_LEFT
tpToCInt TPRight    = B.c'POTRACE_TURNPOLICY_RIGHT
tpToCInt TPMinority = B.c'POTRACE_TURNPOLICY_MINORITY
tpToCInt TPMajority = B.c'POTRACE_TURNPOLICY_MAJORITY
tpToCInt TPRandom   = B.c'POTRACE_TURNPOLICY_RANDOM

withTurnPolicy :: TurnPolicy -> Options -> Options
withTurnPolicy tp = withOption B.p'potrace_param_s'turnpolicy (tpToCInt tp)

withAlphaMax :: Double -> Options -> Options
withAlphaMax alpha = withOption B.p'potrace_param_s'alphamax (realToFrac alpha)

withOptimization :: Maybe Double -> Options -> Options
withOptimization Nothing = withOption B.p'potrace_param_s'opticurve 0
withOptimization (Just tolerance) = withOption B.p'potrace_param_s'opticurve 1 .
                                    withOption B.p'potrace_param_s'opttolerance (realToFrac tolerance)

potraceVersion :: String
potraceVersion = unsafePerformIO $ peekCString =<< B.c'potrace_version
