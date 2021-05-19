{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Tuple where

import Trace
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

instance IsTuple (a, b) where
  -- type TupleR (a, b) = (((), a), b)
  -- fromTup (a, b)     = (((), a), b)
  -- toTup (((), a), b) = (a, b)

instance IsTuple (a, b, c) where
  -- type TupleR (a, b, c)   = (TupleR (a, b), c)
  -- fromTup (a, b, c)       = ((((), a), b), c)
  -- toTup ((((), a), b), c) = (a, b, c)


class GIsTuple f where
  type GTupleR t f
  gfromTup :: t -> f a -> GTupleR t f
  gtoTup   :: GTupleR t f -> (t, f a)

instance GIsTuple U1 where
  type GTupleR t U1 = t
  gfromTup t U1 = t
  gtoTup t = (t, U1)

instance GIsTuple a => GIsTuple (M1 i c a) where
  type GTupleR t (M1 i c a) = GTupleR t a
  gfromTup t (M1 x) = gfromTup t x
  gtoTup x  = let (t, x1) = gtoTup x in (t, M1 x1)

instance GIsTuple (K1 i a) where
  type GTupleR t (K1 i a) = (t, a)
  gfromTup t (K1 x) = (t, x)
  gtoTup (t, x) = (t, K1 x)

instance (GIsTuple a, GIsTuple b) => GIsTuple (a :*: b) where
  type GTupleR t (a :*: b) = GTupleR (GTupleR t a) b
  gfromTup t (a :*: b) = gfromTup (gfromTup t a) b
  gtoTup t0 =
    let (t1, b) = gtoTup t0
        (t2, a) = gtoTup t1
     in
     (t2, a :*: b)

instance (GIsTuple a, GIsTuple b, GIsSumTuple (a :+: b)) => GIsTuple (a :+: b) where
  type GTupleR t (a :+: b) = (TAG, GSumTupleR t (a :+: b))
  -- gfromTup t = gfromTup @(t :*: (a :+: b)) . gfromSumTup t
  -- gfromTup t (a :+: b) = gfromSumTup (gfromTup t a) b

class GIsSumTuple f where
  type GSumTupleR t f
  gfromSumTup :: t -> f a -> GSumTupleR t f
  gtoSumTup :: GSumTupleR t f -> (t, f a)

instance GIsSumTuple U1 where
  type GSumTupleR t U1 = t
  gfromSumTup t U1 = t
  gtoSumTup t = (t, U1)

instance GIsSumTuple a => GIsSumTuple (M1 i c a) where
  type GSumTupleR t (M1 i c a) = GSumTupleR t a
  gfromSumTup t (M1 x) = gfromSumTup t x
  gtoSumTup x  = let (t, x1) = gtoSumTup x in (t, M1 x1)

instance GIsSumTuple (K1 i a) where
  type GSumTupleR t (K1 i a) = (t, a)
  gfromSumTup t (K1 x) = (t, x)
  gtoSumTup (t, x) = (t, K1 x)

instance (GIsTuple a, GIsTuple b) => GIsSumTuple (a :*: b) where
  type GSumTupleR t (a :*: b) = GTupleR t (a :*: b)
  gfromSumTup = gfromTup @(a :*: b)
  gtoSumTup t0 =
    let (t1, b) = gtoTup t0
        (t2, a) = gtoTup t1
     in
     (t2, a :*: b)

instance (GIsSumTuple a, GIsSumTuple b) => GIsSumTuple (a :+: b) where
  type GSumTupleR t (a :+: b) = GTupleR (GTupleR t a) b
--   gfromSumTup t (a :*: b) = gfromSumTup (gfromSumTup t a) b
--   gtoSumTup t0 =
--     let (t1, b) = gtoSumTup t0
--         (t2, a) = gtoSumTup t1
--      in
--      (t2, a :*: b)

