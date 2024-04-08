{-# LANGUAGE DataKinds, MagicHash, RoleAnnotations #-}
module Data.Ref.Impl (module Data.Ref.Impl) where

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
data Ref r a = Ref (MutVar# RealWorld a)

-- It's nice that two references must clearly have the same lifetime to be equal
instance Eq (Ref r a) where
  Ref ref1# == Ref ref2# = isTrue# (sameMutVar# ref1# ref2#)
