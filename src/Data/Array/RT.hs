{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds, MagicHash, RoleAnnotations, UnboxedTuples, RankNTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Data.Array.RT (module Data.Array.RT) where

import Control.Monad.RT (RT(RT))

import GHC.Base (MutableArray#, RealWorld, sameMutableArray#, newArray#, readArray#, writeArray#,
                 isTrue#, unsafeThawArray#, (+#), (==#), indexArray#, unsafeFreezeArray#, Int(I#))
import GHC.Arr (Array(Array), Ix, safeRangeSize, arrEleBottom, safeIndex)

type role RTArray nominal nominal representational
data RTArray r i e = RTArray !i -- the lower bound, l
                             !i -- the upper bound, u
                             {-# UNPACK #-} !Int -- the size of the array
                             (MutableArray# RealWorld e)

instance Eq (RTArray r i e) where
  RTArray _ _ _ arr1# == RTArray _ _ _ arr2# = isTrue# (sameMutableArray# arr1# arr2#)

buildArray :: Ix i => (i, i) -> e -> (forall r. RTArray r i e -> RT ()) -> RT (Array i e)
buildArray range initial k = newRTArray range initial (\arr -> k arr >> unsafeFreezeRTArray arr)

newRTArray :: Ix i => (i, i) -> e -> (forall r. RTArray r i e -> RT a) -> RT a
newRTArray (l, u) initial k = RT $ \s1# ->
    case safeRangeSize (l, u) of
      n@(I# n#) -> case newArray# n# initial s1# of
        (# s2#, marr# #) -> let RT k' = k (RTArray l u n marr#) in k' s2#

{-# INLINE getBoundsRTArray #-}
getBoundsRTArray :: RTArray r i e -> RT (i, i)
getBoundsRTArray (RTArray l u _ _) = return $! (l,u)

{-# INLINE getNumElementsRTArray #-}
getNumElementsRTArray :: RTArray r i e -> RT Int
getNumElementsRTArray (RTArray _ _ sz _) = return $! sz

{-# INLINE unsafeReadRTArray #-}
unsafeReadRTArray :: RTArray r i e -> Int -> RT e
unsafeReadRTArray (RTArray _ _ _ marr#) (I# i#) = RT $ \s# -> readArray# marr# i# s#

{-# INLINE unsafeWriteRTArray #-}
unsafeWriteRTArray :: RTArray r i e -> Int -> e -> RT ()
unsafeWriteRTArray  (RTArray _ _ _ marr#) (I# i#) x = RT $ \s1# ->
  case writeArray# marr# i# x s1# of
    s2# -> (# s2#, () #)

{-# INLINE readRTArray #-}
readRTArray :: Ix i => RTArray r i e -> i -> RT e
readRTArray marr@(RTArray l u n _) i = unsafeReadRTArray marr (safeIndex (l, u) n i)

{-# INLINE writeRTArray #-}
writeRTArray :: Ix i => RTArray r i e -> i -> e -> RT ()
writeRTArray marr@(RTArray l u n _) i x = unsafeWriteRTArray marr (safeIndex (l, u) n i) x

freezeRTArray :: RTArray r i e -> RT (Array i e)
freezeRTArray (RTArray l u n@(I# n#) marr#) = RT $ \s1# ->
    case newArray# n# arrEleBottom s1#  of
      (# s2#, marr'# #) ->
        let copy i# s#
              | isTrue# (i# ==# n#) = s#
              | otherwise = case readArray# marr# i# s# of
                  (# s'#, e #) -> copy (i# +# 1#) (writeArray# marr'# i# e s'#)
            s3# = copy 0# s2#
        in case unsafeFreezeArray# marr'# s3# of
             (# s4#, arr# #) -> (# s4#, Array l u n arr# #)

{-# INLINE unsafeFreezeRTArray #-}
unsafeFreezeRTArray :: RTArray r i e -> RT (Array i e)
unsafeFreezeRTArray (RTArray l u n marr#) = RT $ \s1# ->
    case unsafeFreezeArray# marr# s1# of
      (# s2#, arr# #) -> (# s2#, Array l u n arr# #)

thawRTArray :: Array i e -> RT (RTArray r i e)
thawRTArray (Array l u n@(I# n#) arr#) = RT $ \s1# ->
    case newArray# n# arrEleBottom s1#  of
      (# s2#, marr# #) ->
        let copy i# s#
              | isTrue# (i# ==# n#) = s#
              | otherwise = case indexArray# arr# i# of
                  (# e #) -> copy (i# +# 1#) (writeArray# marr# i# e s#)
            s3# = copy 0# s2#
        in (# s3#, RTArray l u n marr# #)

{-# INLINE unsafeThawRTArray #-}
unsafeThawRTArray :: Array i e -> RT (RTArray r i e)
unsafeThawRTArray (Array l u n arr#) = RT $ \s1# ->
    case unsafeThawArray# arr# s1# of
      (# s2#, marr# #) -> (# s2#, RTArray l u n marr# #)
