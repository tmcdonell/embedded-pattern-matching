{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Elt (
  module Elt,
  TAG,
) where

import Rec
import Type
import Trace
import Tuple

import Type.Reflection


class Typeable a => Elt a where
  type EltR a

  toElt   :: EltR a -> a
  fromElt :: a -> EltR a
  eltR    :: TypeR (EltR a)
  traceR  :: [TraceR (EltR a)]

instance Elt () where
  type EltR () = ()
  toElt   = id
  fromElt = id
  eltR    = TypeRunit
  traceR  = [TraceRunit]

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
  type EltR (a, b) = TupleR (EltR a, EltR b)
  toElt (((), a), b) = (toElt a, toElt b)
  fromElt (a, b)     = (((), fromElt a), fromElt b)
  eltR               = TypeRunit `TypeRpair` eltR @a `TypeRpair` eltR @b
  traceR             = [TraceRunit `TraceRpair` a `TraceRpair` b | a <- traceR @a, b <- traceR @b ]

instance (Elt a, Elt b, Elt c) => Elt (a, b, c) where
  type EltR (a, b, c) = TupleR (EltR a, EltR b, EltR c)
  toElt ((((), a), b), c) = (toElt a, toElt b, toElt c)
  fromElt (a, b, c)       = ((((), fromElt a), fromElt b), fromElt c)
  eltR                    = TypeRunit `TypeRpair` eltR @a `TypeRpair` eltR @b `TypeRpair` eltR @c
  traceR                  = [TraceRunit `TraceRpair` a `TraceRpair` b `TraceRpair` c | a <- traceR @a, b <- traceR @b, c <- traceR @c ]

instance Elt a => Elt (Rec a) where
  type EltR (Rec a) = Rec a
  fromElt = id
  toElt   = id
  eltR    = TypeRrec typeRep
  traceR  = [TraceRrec typeRep]

