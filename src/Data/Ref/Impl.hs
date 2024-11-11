{-# LANGUAGE DataKinds, MagicHash, RoleAnnotations #-}
{-|
Module      : Data.Ref.Impl
Description : The implemenation of mutable references for the 'Control.Monad.RT.RT' monad.
License     : BSD-3-Clause
Maintainer  : Jamie Willis
Stability   : stable

This module provides (the implementation of) mutable references for the 'Control.Monad.RT.RT' monad.

@since 0.1.0.0
-}
module Data.Ref.Impl (
  -- * Refs
  Ref(Ref)
) where

import GHC.Base (MutVar#, RealWorld, sameMutVar#, isTrue#)

{-
The `r` parameter is deliberately nominal here, which prevents the use of
coerce to "safely" escape the lifetime of a reference:

> escape :: a -> RT (Ref r a)
> escape x = newRef x (return . coerce)

Would compile if the annotation was left as phantom (which is the inferred).
This is clearly not something we want to allow, so nominal it is.
-}
type role Ref nominal representational
-- Don't even expose the constructor, then it's pretty much safe?
{-|
A @'Ref' r a@ describes a mutable variable with lifetime @r@, containing a value of type @a@.
Primarily used within the 'Control.Monad.RT.RT' monad.

The lifetime @r@ is provided at reference creation in 'Data.Ref.newRef'.
This is distinct from 'Data.STRef.STRef's, whose \'lifetime\' is provided by the 'Control.Monad.ST.ST' 
monad.

@since 0.1.0.0
-}
data Ref r a = Ref (MutVar# RealWorld a)

-- It's nice that two references must clearly have the same lifetime to be equal
instance Eq (Ref r a) where
  Ref ref1# == Ref ref2# = isTrue# (sameMutVar# ref1# ref2#)
