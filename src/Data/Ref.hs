{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash, UnboxedTuples, RankNTypes, TypeOperators #-}
{-|
Module      : Data.Ref
Description : Mutable references for the 'Control.Monad.RT.RT' monad.
License     : BSD-3-Clause
Maintainer  : Jamie Willis
Stability   : stable

This module provides mutable references for the 'Control.Monad.RT.RT' monad.

@since 0.1.0.0
-}
module Data.Ref (
  -- * References
  Ref,
  -- * Reference Operations
  newRef,
  readRef,
  writeRef,
  fromIORef,
  lifetimeEqual
  ) where

import Control.Monad.RT.Unsafe (RT(RT))
import Data.Ref.Impl (Ref(Ref))

import GHC.Base (newMutVar#, readMutVar#, writeMutVar#)

import GHC.IORef (IORef(IORef))
import GHC.STRef (STRef(STRef))

import Data.Type.Equality ((:~:)(Refl))
import Unsafe.Coerce (unsafeCoerce)


{-|
Create a new 'Ref' with value @x@, which the computation @k@ has access to.

@since 0.1.0.0
-}
{-# INLINABLE newRef #-}
newRef  :: a  -- ^ @x@, The initial value of the reference
        -> (forall r. Ref r a -> RT b) 
              -- ^ @k@, the computation parameterised by the new reference.
        -> RT b
              -- ^ An 'RT' computation which runs @k@ with its reference initialised to value @x@.
newRef x k = RT $ \s# ->
  case newMutVar# x s# of
    (# s'#, ref# #) -> let RT k' = k (Ref ref#) in k' s'#

{-|
Read the current value of the 'Ref'.

@since 0.1.0.0
-}
{-# INLINE readRef #-}
readRef :: Ref r a -- ^ @ref@, the reference whose value we wish to read.
        -> RT a    -- ^ A computation which returns the current value of @ref@.
readRef (Ref ref#) = RT $ \s# -> readMutVar# ref# s#

{-|
Write a new value into a 'Ref', replacing the old value.

@since 0.1.0.0
-}
{-# INLINABLE writeRef #-}
writeRef  :: Ref r a -- ^ @ref@, the reference whose value we wish to replace
          -> a       -- ^ @x@, the new value of the reference
          -> RT ()   -- ^ A computation which updates the value of @ref@ to @x@.
writeRef (Ref ref#) x = RT $ \s# ->
  case writeMutVar# ref# x s# of
    s'# -> (# s'#, () #)

{-|
Convert an 'IORef' to a 'Ref'.

@since 0.1.0.0
-}
-- unsafeFromIORef?
{-# INLINE fromIORef #-}
fromIORef :: IORef a -> Ref r a
fromIORef (IORef (STRef ref#)) = Ref ref#

{-|
Returns a proof that if two references are equal, they must have the same lifetime.

This illustrates that references are identified precisely by their lifetimes.

@since 0.1.0.0
-}
lifetimeEqual :: Ref r1 a -> Ref r2 a -> Maybe (r1 :~: r2)
lifetimeEqual (Ref ref1#) ref2
  | Ref ref1# == ref2 = Just (unsafeCoerce Refl)
  | otherwise         = Nothing
