{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Control.Monad.RT (RT, module Control.Monad.RT) where

import Control.Monad.RT.Unsafe

import GHC.Base (runRW#)
import GHC.IO (IO(IO))
import Data.Coerce (coerce)

{-# INLINE runRT #-}
runRT :: RT a -> a
runRT (RT mx) = case runRW# mx of (# _, x #) -> x

{-# INLINE rtToIO #-}
rtToIO :: RT a -> IO a
rtToIO = coerce
