{-# LANGUAGE Unsafe #-}
{-# LANGUAGE UnboxedTuples, MagicHash, DerivingVia, DataKinds #-}
{-|
Module      : Control.Monad.RT.Unsafe
Description : Support for strict reference threads, and unsafe conversion from 'IO' to 'RT'.
License     : BSD-3-Clause
Maintainer  : Jamie Willis
Stability   : stable

This module provides support for strict reference threads, and unsafe conversion from 'IO' to 'RT'.

@since 0.1.0.0
-}
module Control.Monad.RT.Unsafe (
  -- * The 'RT' Monad
  RT(RT),
  -- * Unsafe Operations
  -- ** Converting 'IO' to 'RT'
  unsafeIOToRT
  ) where

import GHC.Base (State#, RealWorld)
import GHC.IO (IO(IO))
import Data.Coerce (coerce)

{-|
The strict 'RT' (reference-thread) monad.

This is a more fine-grained version of state threads ('Control.Monad.ST.ST').
Where 'Control.Monad.ST.ST' has a type parameter to represent the lifetime of the overall stateful 
computation, 'RT' parameterises its references with unique lifetimes.
This means the overall computation need not have a lifetime parameter, making 'RT' more suitable 
for use in libraries which do not wish to expose the stateful-ness to the user.

That the computation need not have a lifetime parameter is made explicit by 'Control.Monad.RT.runRT',
in which the stateful computation @'RT' a@ is *only* parameterised by the type of the return value.
This may be compared with 'Control.Monad.ST.ST' in the table below.

+--------------------+---------------------------+--------------------------------------------+
| Operation          | 'Control.Monad.ST.ST'     | 'RT'                                       |
+====================+===========================+============================================+
| Monad running      | @(forall s. ST s a) -> a@ | @RT a -> a@                                |
+--------------------+---------------------------+--------------------------------------------+
| Reference Creation | @a -> ST s (STRef s a)@   | @a -> (forall r. Ref r a -> RT b) -> RT b@ |
+--------------------+---------------------------+--------------------------------------------+

Under the hood, 'RT' is essentially an 'Control.Monad.ST.ST' monad specialised to the parameter
'RealWorld'. 
It is because the stateful computation is within 'RealWorld' that we are able to omit it from 
'Control.Monad.RT.runRT'.

@since 0.1.0.0
-}
-- do the instances by hand?
newtype RT a = RT (State# RealWorld -> (# State# RealWorld, a #))
  deriving (Functor, Applicative, Monad) via IO

{-|
Convert an 'IO' action into a 'RT' action.

This relies on 'IO' and 'RT' having the same underlying representation.

@since 0.1.0.0
-}
-- Is this unsafe?
{-# INLINE unsafeIOToRT #-}
unsafeIOToRT :: IO a -> RT a
unsafeIOToRT = coerce
