{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
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
  deriving (Show, Generic, Elt, IsTuple)

pattern Point_ :: Exp Float -> Exp Float -> Exp Point
pattern Point_ x y = Pattern (x, y)
{-# COMPLETE Point_ #-}

liftPoint :: Point -> Exp Point
liftPoint (Point x y) =
  Tuple $ Unit `Pair` Exp (Const x) `Pair` Exp (Const y)

xcoord :: Exp Point -> Exp Float
xcoord x = Prj (PrjL (PrjR PrjZ)) x


data V2 a = V2 a a
  deriving (Show, Generic, Elt, IsTuple)

pattern V2_ :: Elt a => Exp a -> Exp a -> Exp (V2 a)
pattern V2_ x y = Pattern (x, y)
{-# COMPLETE V2_ #-}

