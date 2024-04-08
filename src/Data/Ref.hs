{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash, UnboxedTuples, RankNTypes, TypeOperators #-}
module Data.Ref (Ref, module Data.Ref) where

import Control.Monad.RT.Unsafe (RT(RT))
import Data.Ref.Impl (Ref(Ref))

import GHC.Base (newMutVar#, readMutVar#, writeMutVar#)

import GHC.IORef (IORef(IORef))
import GHC.STRef (STRef(STRef))

import Data.Type.Equality ((:~:)(Refl))
import Unsafe.Coerce (unsafeCoerce)


{-# INLINABLE newRef #-}
newRef :: a -> (forall r. Ref r a -> RT b) -> RT b
newRef x k = RT $ \s# ->
  case newMutVar# x s# of
    (# s'#, ref# #) -> let RT k' = k (Ref ref#) in k' s'#

{-# INLINE readRef #-}
readRef :: Ref r a -> RT a
readRef (Ref ref#) = RT $ \s# -> readMutVar# ref# s#

{-# INLINABLE writeRef #-}
writeRef :: Ref r a -> a -> RT ()
writeRef (Ref ref#) x = RT $ \s# ->
  case writeMutVar# ref# x s# of
    s'# -> (# s'#, () #)

-- unsafeFromIORef?
{-# INLINE fromIORef #-}
fromIORef :: IORef a -> Ref r a
fromIORef (IORef (STRef ref#)) = Ref ref#

-- returns a proof that if two references are equal, they must have the same lifetime
lifetimeEqual :: Ref r1 a -> Ref r2 a -> Maybe (r1 :~: r2)
lifetimeEqual (Ref ref1#) ref2
  | Ref ref1# == ref2 = Just (unsafeCoerce Refl)
  | otherwise         = Nothing
