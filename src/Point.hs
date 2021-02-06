{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Point where

import Elt
import Exp
import Pattern
import Tuple

data Point = Point Float Float

instance Elt Point where
  type EltR Point = EltR (Float, Float)

  toElt p = let (x, y) = toElt p
             in Point x y
  fromElt (Point x y) = fromElt (x,y)

  eltR = eltR @(Float, Float)
  traceR = traceR @(Float, Float)

-- type instance TupleR Point = TupleR (Float, Float)

instance IsTuple Point where
  type TupleR Point = TupleR (Float, Float)
  fromTup (Point x y) = fromTup (x, y)
  toTup t = let (x,y) = toTup t
             in Point x y

pattern Point_ :: Exp Float -> Exp Float -> Exp Point
pattern Point_ x y = Pattern (x, y)
{-# COMPLETE Point_ #-}

