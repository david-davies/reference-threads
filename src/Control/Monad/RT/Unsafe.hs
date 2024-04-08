{-# LANGUAGE Unsafe #-}
{-# LANGUAGE UnboxedTuples, MagicHash, DerivingVia, DataKinds #-}
module Control.Monad.RT.Unsafe (module Control.Monad.RT.Unsafe) where

import GHC.Base (State#, RealWorld)
import GHC.IO (IO(IO))
import Data.Coerce (coerce)

-- do the instances by hand?
newtype RT a = RT (State# RealWorld -> (# State# RealWorld, a #))
  deriving (Functor, Applicative, Monad) via IO

{-# INLINE unsafeIOToRT #-}
unsafeIOToRT :: IO a -> RT a
unsafeIOToRT = coerce
