{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds, MagicHash, RoleAnnotations, UnboxedTuples, RankNTypes #-}
module Data.Ref (module Data.Ref) where

import Control.Monad.RT (RT(RT))

import GHC.Base (MutVar#, RealWorld, newMutVar#, readMutVar#, writeMutVar#)

import GHC.IORef (IORef(IORef))
import GHC.STRef (STRef(STRef))

type role Ref phantom representational
-- Don't even expose the constructor, then it's pretty much safe?
data Ref r a = Ref (MutVar# RealWorld a)

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
