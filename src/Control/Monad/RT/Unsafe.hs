module Control.Monad.RT.Unsafe (module Control.Monad.RT.Unsafe) where

import Control.Monad.RT (RT(RT))
import GHC.IO (IO(IO))
import Data.Coerce (coerce)

{-# INLINE unsafeIOToRT #-}
unsafeIOToRT :: IO a -> RT a
unsafeIOToRT = coerce
