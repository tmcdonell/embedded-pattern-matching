{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}

module Elt (
  Elt(..),
  Generic,
  TAG,
) where

import Rec
import Type
import Trace

import Data.Bits
import GHC.Generics

import Type.Reflection


class Typeable a => Elt a where
  type EltR a
  type EltR a = GEltR () (Rep a)
  --
  toElt   :: EltR a -> a
  fromElt :: a -> EltR a
  eltR    :: TypeR (EltR a)
  traceR  :: [TraceR (EltR a)]

  default fromElt
    :: (Generic a, GElt (Rep a), EltR a ~ GEltR () (Rep a))
    => a
    -> EltR a
  fromElt = gfromElt () . from

  default toElt
    :: (Generic a, GElt (Rep a), EltR a ~ GEltR () (Rep a))
    => EltR a
    -> a
  toElt = to . snd . gtoElt @(Rep a) @()

  default traceR
    :: (Generic a, GElt (Rep a), EltR a ~ GEltR () (Rep a))
    => [TraceR (EltR a)]
  traceR = gtraceR @(Rep a) TraceRunit

  default eltR
    :: (GElt (Rep a), EltR a ~ GEltR () (Rep a))
    => TypeR (EltR a)
  eltR = geltR @(Rep a) TypeRunit


instance Elt () where
  -- type EltR () = ()
  -- toElt   = id
  -- fromElt = id
  -- eltR    = TypeRunit
  -- traceR  = [TraceRunit]

instance Elt Int where
  type EltR Int = Int
  toElt   = id
  fromElt = id
  eltR    = TypeRprim primType
  traceR  = [TraceRprim primType]

instance Elt Float where
  type EltR Float = Float
  toElt   = id
  fromElt = id
  eltR    = TypeRprim primType
  traceR  = [TraceRprim primType]

instance Elt TAG where
  type EltR TAG = TAG
  toElt   = id
  fromElt = id
  eltR    = TypeRprim primType
  traceR  = [TraceRprim primType]

instance (Elt a, Elt b) => Elt (a, b) where
  -- type EltR (a, b) = TupleR (EltR a, EltR b)
  -- toElt (((), a), b) = (toElt a, toElt b)
  -- fromElt (a, b)     = (((), fromElt a), fromElt b)
  -- eltR               = TypeRunit `TypeRpair` eltR @a `TypeRpair` eltR @b
  -- traceR             = [TraceRunit `TraceRpair` a `TraceRpair` b | a <- traceR @a, b <- traceR @b ]

instance (Elt a, Elt b, Elt c) => Elt (a, b, c) where
  -- type EltR (a, b, c) = TupleR (EltR a, EltR b, EltR c)
  -- toElt ((((), a), b), c) = (toElt a, toElt b, toElt c)
  -- fromElt (a, b, c)       = ((((), fromElt a), fromElt b), fromElt c)
  -- eltR                    = TypeRunit `TypeRpair` eltR @a `TypeRpair` eltR @b `TypeRpair` eltR @c
  -- traceR                  = [TraceRunit `TraceRpair` a `TraceRpair` b `TraceRpair` c | a <- traceR @a, b <- traceR @b, c <- traceR @c ]

instance Elt a => Elt (Rec a) where
  type EltR (Rec a) = Rec a
  fromElt = id
  toElt   = id
  eltR    = TypeRrec (eltR @a)
  traceR  = [TraceRrec (eltR @a)]


class GElt f where
  type GEltR t f
  geltR    :: TypeR t -> TypeR (GEltR t f)
  gfromElt :: t -> f a -> GEltR t f
  gtoElt   :: GEltR t f -> (t, f a)
  gundef   :: t -> GEltR t f
  gtraceR  :: TraceR t -> [TraceR (GEltR t f)]
  gundefR  :: TraceR t -> TraceR (GEltR t f)

instance GElt U1 where
  type GEltR t U1 = t
  geltR t       = t
  gfromElt t U1 = t
  gtoElt   t    = (t, U1)
  gundef   t    = t
  gtraceR t     = [t]
  gundefR t     = t

instance GElt a => GElt (M1 i c a) where
  type GEltR t (M1 i c a) = GEltR t a
  geltR             = geltR @a
  gfromElt t (M1 x) = gfromElt t x
  gtoElt         x  = let (t, x1) = gtoElt x in (t, M1 x1)
  gundef t          = gundef @a t
  gtraceR           = gtraceR @a
  gundefR           = gundefR @a

instance Elt a => GElt (K1 i a) where
  type GEltR t (K1 i a) = (t, EltR a)
  geltR t           = TypeRpair t (eltR @a)
  gfromElt t (K1 x) = (t, fromElt x)
  gtoElt     (t, x) = (t, K1 (toElt x))
  gundef t          = (t, undef (eltR @a))
  gtraceR t         = TraceRpair t <$> traceR @a
  gundefR t         = TraceRpair t (untrace (eltR @a))

instance (GElt a, GElt b) => GElt (a :*: b) where
  type GEltR t (a :*: b) = GEltR (GEltR t a) b
  geltR             = geltR @b . geltR @a
  gfromElt t (a :*: b) = gfromElt (gfromElt t a) b
  gtoElt t =
    let (t1, b) = gtoElt t
        (t2, a) = gtoElt t1
    in
    (t2, a :*: b)
  gundef t = gundef @b (gundef @a t)
  gtraceR = concatMap (gtraceR @b) . gtraceR @a
  gundefR = gundefR @b . gundefR @a

instance (GElt a, GElt b, GSumElt (a :+: b)) => GElt (a :+: b) where
  type GEltR t (a :+: b) = (TAG, GSumEltRepr t (a :+: b))
  geltR t      = TypeRpair (TypeRprim primType) (gsumEltR @(a :+: b) t)
  gfromElt     = gsumFromElt 0
  gtoElt (k,x) = gsumToElt k x
  gundef t     = (0xff, gsumUndef @(a :+: b) t)
  gtraceR t    = uncurry TraceRtag <$> gsumTraceR @(a :+: b) 0 t
  gundefR t    = TraceRpair (TraceRundef (eltR @TAG)) (gsumUndefR @(a :+: b) t)


class GSumElt (f :: * -> *) where
  type GSumEltRepr t f
  gsumEltR    :: TypeR t -> TypeR (GSumEltRepr t f)
  gsumFromElt :: TAG -> t -> f a -> (TAG, GSumEltRepr t f)
  gsumToElt   :: TAG -> GSumEltRepr t f -> (t, f a)
  gsumUndef   :: t -> GSumEltRepr t f
  gsumTraceR  :: TAG -> TraceR t -> [(TAG, TraceR (GSumEltRepr t f))]
  gsumUndefR  :: TraceR t -> TraceR (GSumEltRepr t f)

instance GSumElt U1 where
  type GSumEltRepr t U1 = t
  gsumEltR t         = t
  gsumFromElt n t U1 = (n, t)
  gsumToElt _ t      = (t, U1)
  gsumUndef t        = t
  gsumTraceR n t     = [(n, t)]
  gsumUndefR t       = t

instance GSumElt a => GSumElt (M1 i c a) where
  type GSumEltRepr t (M1 i c a) = GSumEltRepr t a
  gsumEltR               = gsumEltR @a
  gsumFromElt n t (M1 x) = gsumFromElt n t x
  gsumToElt k x          = let (t, x') = gsumToElt k x in (t, M1 x')
  gsumUndef              = gsumUndef @a
  gsumTraceR             = gsumTraceR @a
  gsumUndefR             = gsumUndefR @a

instance Elt a => GSumElt (K1 i a) where
  type GSumEltRepr t (K1 i a) = (t, EltR a)
  gsumEltR t             = TypeRpair t (eltR @a)
  gsumFromElt n t (K1 x) = (n, (t, fromElt x))
  gsumToElt _ (t, x)     = (t, K1 (toElt x))
  gsumUndef t            = (t, undef (eltR @a))
  gsumTraceR n t         = (n,) . TraceRpair t <$> traceR @a
  gsumUndefR t           = TraceRpair t (untrace (eltR @a))

instance (GElt a, GElt b) => GSumElt (a :*: b) where
  type GSumEltRepr t (a :*: b) = GEltR t (a :*: b)
  gsumEltR = geltR @(a :*: b)
  gsumFromElt n t (a :*: b) = (n, gfromElt (gfromElt t a) b)
  gsumUndef t = gundef @b (gundef @a t)
  gsumToElt _ t0 =
    let (t1, b) = gtoElt t0
        (t2, a) = gtoElt t1
     in
     (t2, a :*: b)
  gsumTraceR n t = (n,) <$> gtraceR @(a :*: b) t
  gsumUndefR = gundefR @(a :*: b)

instance (GSumElt a, GSumElt b) => GSumElt (a :+: b) where
  type GSumEltRepr t (a :+: b) = GSumEltRepr (GSumEltRepr t a) b
  gsumEltR = gsumEltR @b . gsumEltR @a
  gsumUndef t = gsumUndef @b (gsumUndef @a t)

  gsumFromElt n t (L1 a) = let (m,r) = gsumFromElt n t a
                            in (shiftL m 1, gsumUndef @b r)
  gsumFromElt n t (R1 b) = let (m,r) = gsumFromElt n (gsumUndef @a t) b
                            in (setBit (m `shiftL` 1) 0, r)

  gsumToElt k t0 =
    let (t1, b) = gsumToElt (shiftR k 1) t0
        (t2, a) = gsumToElt (shiftR k 1) t1
     in
     if testBit k 0
        then (t2, R1 b)
        else (t2, L1 a)

  gsumTraceR k t =
    let a = gsumTraceR @a k t
        b = gsumTraceR @b k (gsumUndefR @a t)
     in
     map (\(x,y) ->         (x `shiftL` 1,    gsumUndefR @b y)) a ++
     map (\(x,y) -> (setBit (x `shiftL` 1) 0, y)) b

  gsumUndefR = gsumUndefR @b . gsumUndefR @a

