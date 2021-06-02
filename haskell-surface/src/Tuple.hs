{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Tuple where

import Type
import Elt

import Data.Bits
import GHC.Generics

class IsTuple t where
  type TupleR t
  type TupleR t = GTupleR () (Rep t)

  fromTup :: t -> TupleR t
  toTup   :: TupleR t -> t

  default fromTup
      :: (Generic t, TupleR t ~ GTupleR () (Rep t), GIsTuple (Rep t))
      => t
      -> TupleR t
  fromTup = gfromTup @(Rep t) () . from

  default toTup
      :: (Generic t, TupleR t ~ GTupleR () (Rep t), GIsTuple (Rep t))
      => TupleR t
      -> t
  toTup = to . snd . gtoTup @(Rep t) @()


instance IsTuple () where
  -- type TupleR () = ()
  -- fromTup        = id
  -- toTup          = id

instance (Elt a, Elt b) => IsTuple (a, b) where
  -- type TupleR (a, b) = (((), a), b)
  -- fromTup (a, b)     = (((), a), b)
  -- toTup (((), a), b) = (a, b)

instance (Elt a, Elt b, Elt c) => IsTuple (a, b, c) where
  -- type TupleR (a, b, c)   = (TupleR (a, b), c)
  -- fromTup (a, b, c)       = ((((), a), b), c)
  -- toTup ((((), a), b), c) = (a, b, c)


class GIsTuple f where
  type GTupleR t f
  gfromTup :: t -> f a -> GTupleR t f
  gtoTup   :: GTupleR t f -> (t, f a)
  gundef   :: t -> GTupleR t f

instance GIsTuple U1 where
  type GTupleR t U1 = t
  gfromTup t U1 = t
  gtoTup t = (t, U1)
  gundef t = t

instance GIsTuple a => GIsTuple (M1 i c a) where
  type GTupleR t (M1 i c a) = GTupleR t a
  gfromTup t (M1 x) = gfromTup t x
  gtoTup x  = let (t, x1) = gtoTup x in (t, M1 x1)
  gundef = gundef @a

instance Elt a => GIsTuple (K1 i a) where
  type GTupleR t (K1 i a) = (t, a)
  gfromTup t (K1 x) = (t, x)
  gtoTup (t, x) = (t, K1 x)
  gundef t = (t, toElt (undef (eltR @a)))

instance (GIsTuple a, GIsTuple b) => GIsTuple (a :*: b) where
  type GTupleR t (a :*: b) = GTupleR (GTupleR t a) b
  gfromTup t (a :*: b) = gfromTup (gfromTup t a) b
  gtoTup t0 =
    let (t1, b) = gtoTup t0
        (t2, a) = gtoTup t1
     in
     (t2, a :*: b)
  gundef t = gundef @b (gundef @a t)

instance (GIsTuple a, GIsTuple b, GIsSumTuple (a :+: b)) => GIsTuple (a :+: b) where
  type GTupleR t (a :+: b) = (TAG, GSumTupleR t (a :+: b))
  gfromTup = gsumFromTup 0
  gtoTup (k,x) = gsumToTup k x
  gundef t = (0xff, gsumUndef @(a :+: b) t)

class GIsSumTuple f where
  type GSumTupleR t f
  gsumFromTup :: TAG -> t -> f a -> (TAG, GSumTupleR t f)
  gsumToTup   :: TAG -> GSumTupleR t f -> (t, f a)
  gsumUndef   :: t -> GSumTupleR t f

instance GIsSumTuple U1 where
  type GSumTupleR t U1 = t
  gsumFromTup n t U1 = (n, t)
  gsumToTup _ t = (t, U1)
  gsumUndef t = t

instance GIsSumTuple a => GIsSumTuple (M1 i c a) where
  type GSumTupleR t (M1 i c a) = GSumTupleR t a
  gsumFromTup n t (M1 x) = gsumFromTup n t x
  gsumToTup k x  = let (t, x') = gsumToTup k x in (t, M1 x')
  gsumUndef = gsumUndef @a

instance Elt a => GIsSumTuple (K1 i a) where
  type GSumTupleR t (K1 i a) = (t, a)
  gsumFromTup n t (K1 x) = (n, (t, x))
  gsumToTup _ (t, x) = (t, K1 x)
  gsumUndef t = (t, toElt (undef (eltR @a)))

instance (GIsTuple a, GIsTuple b) => GIsSumTuple (a :*: b) where
  type GSumTupleR t (a :*: b) = GTupleR t (a :*: b)
  gsumFromTup n t (a :*: b) = (n, gfromTup (gfromTup t a) b)
  gsumToTup _ t0 =
    let (t1, b) = gtoTup t0
        (t2, a) = gtoTup t1
     in
     (t2, a :*: b)
  gsumUndef t = gundef @b (gundef @a t)

instance (GIsSumTuple a, GIsSumTuple b) => GIsSumTuple (a :+: b) where
  type GSumTupleR t (a :+: b) = GSumTupleR (GSumTupleR t a) b
  gsumFromTup n t (L1 a) = let (m,r) = gsumFromTup n t a
                            in (shiftL m 1, gsumUndef @b r)
  gsumFromTup n t (R1 b) = let (m,r) = gsumFromTup n (gsumUndef @a t) b
                            in (setBit (m `shiftL` 1) 0, r)

  gsumToTup k t0 =
    let (t1, b) = gsumToTup (shiftR k 1) t0
        (t2, a) = gsumToTup (shiftR k 1) t1
     in
     if testBit k 0
        then (t2, R1 b)
        else (t2, L1 a)

  gsumUndef t = gsumUndef @b (gsumUndef @a t)

