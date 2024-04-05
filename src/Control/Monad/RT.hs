{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds, MagicHash, UnboxedTuples, DerivingVia #-}
module Control.Monad.RT (module Control.Monad.RT) where

import GHC.Base (RealWorld, State#, runRW#)
import GHC.IO (IO(IO))
import Data.Coerce (coerce)

-- do the instances by hand?
newtype RT a = RT (State# RealWorld -> (# State# RealWorld, a #))
  deriving (Functor, Applicative, Monad) via IO

{-# INLINE runRT #-}
runRT :: RT a -> a
runRT (RT mx) = case runRW# mx of (# _, x #) -> x

{-# INLINE rtToIO #-}
-- unsafeRTToIO?
rtToIO :: RT a -> IO a
rtToIO = coerce
