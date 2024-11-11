{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-|
Module      : Control.Monad.RT
Description : Support for strict reference threads.
License     : BSD-3-Clause
Maintainer  : Jamie Willis
Stability   : stable

This module provides support for strict reference threads.

'Data.Ref' provides the references, and 'Data.Array.RT' the arrays,
that may be used within the 'RT' monad,

@since 0.1.0.0
-}
module Control.Monad.RT (
  -- * The 'RT' monad
  RT, 
  runRT,
  -- * Converting from 'RT' to 'IO'
  rtToIO
  ) where

import Control.Monad.RT.Unsafe ( RT(..) )

import GHC.Base (runRW#)
import GHC.IO (IO(IO))
import Data.Coerce (coerce)

{-|
Return the value computed by a reference thread. 

Unlike 'Control.Monad.ST.runST', there is no need here to parameterise by the internal state used 
by the 'RT' computation.
Instead, the lifetimes of internal states (references) are parameterised at the time of their 
creation *within* the 'RT' computation. 

@since 0.1.0.0
-}
{-# INLINE runRT #-}
runRT :: RT a -> a
runRT (RT mx) = case runRW# mx of (# _, x #) -> x

{-|
Embed a reference thread in an 'IO' action.

This allows any 'RT' computation to be computed as if it were an 'IO' action.
Moreover, this is safe.

@since 0.1.0.0
-}
{-# INLINE rtToIO #-}
rtToIO :: RT a -> IO a
rtToIO = coerce
