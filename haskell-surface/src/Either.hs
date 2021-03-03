{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Either where

import Elt
import Type
import Trace

instance (Elt a, Elt b) => Elt (Either a b) where
  type EltR (Either a b) = (TAG, EltR (a, b))

  toElt (0,(((),l),_)) = Left (toElt l)
  toElt (1,(((),_),r)) = Right (toElt r)
  toElt _              = error "internal error"

  fromElt (Left l)  = (0,(((),fromElt l), undefined))
  fromElt (Right r) = (1,(((),undefined), fromElt r))

  eltR = eltR @TAG `TypeRpair` eltR @(a, b)
  traceR = [ TraceRtag 0 (TraceRunit `TraceRpair` a `TraceRpair` TraceRundef (eltR @b)) | a <- traceR @a ]
        ++ [ TraceRtag 1 (TraceRunit `TraceRpair` TraceRundef (eltR @a) `TraceRpair` b) | b <- traceR @b ]

