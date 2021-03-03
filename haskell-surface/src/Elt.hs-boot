{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Elt (
  Elt(..)
) where

import {-# SOURCE #-} Type
import {-# SOURCE #-} Trace

import Type.Reflection
import GHC.Generics


class Typeable a => Elt a where
  type EltR a
  type EltR a = GEltR () (Rep a)

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

class GElt f where
  type GEltR t f
  geltR    :: TypeR t -> TypeR (GEltR t f)
  gfromElt :: t -> f a -> GEltR t f
  gtoElt   :: GEltR t f -> (t, f a)
  gundef   :: t -> GEltR t f
  gtraceR  :: TraceR t -> [TraceR (GEltR t f)]
  gundefR  :: TraceR t -> TraceR (GEltR t f)

