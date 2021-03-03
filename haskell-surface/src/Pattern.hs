{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Pattern where

import Elt
import Exp
import Trace
import Tuple


class IsPattern s r where
  construct :: r -> Exp s
  destruct  :: Exp s -> r

pattern Pattern :: IsPattern s r => r -> Exp s
pattern Pattern vars <- (destruct -> vars)
  where Pattern = construct

instance (IsTuple r, TupleR r ~ ((), a), EltR r ~ ((), EltR a)) => IsPattern r (Exp a) where
  construct x = Tuple $ Unit `Pair` Exp (unMatch x)
  destruct r =
    case r of
      Match (TraceRunit `TraceRpair` a) x -> Match a (Prj (PrjR PrjZ) x)
      x                                   -> Prj (PrjR PrjZ) x

instance (IsTuple r, TupleR r ~ TupleR (a, b), EltR r ~ EltR (a, b)) => IsPattern r (Exp a, Exp b) where
  construct (x, y) =
    Tuple $ Unit `Pair` Exp (unMatch x) `Pair` Exp (unMatch y)

  destruct r =
    case r of
      Match (TraceRunit `TraceRpair` a `TraceRpair` b) x
        -> ( Match a (Prj (PrjL (PrjR PrjZ)) x)
           , Match b (Prj (PrjR PrjZ) x)
           )

      x -> ( Prj (PrjL (PrjR PrjZ)) x
           , Prj (PrjR PrjZ) x
           )

instance (IsTuple r, TupleR r ~ ((((), a), b), c), EltR r ~ EltR (a, b, c)) => IsPattern r (Exp a, Exp b, Exp c) where
  construct (x, y, z) =
    Tuple $ Unit `Pair` Exp (unMatch x) `Pair` Exp (unMatch y) `Pair` Exp (unMatch z)

  destruct r =
    case r of
      Match (TraceRunit `TraceRpair` a `TraceRpair` b `TraceRpair` c) x
        -> ( Match a (Prj (PrjL (PrjL (PrjR PrjZ))) x)
           , Match b (Prj (PrjL (PrjR PrjZ)) x)
           , Match c (Prj (PrjR PrjZ) x)
           )

      x -> ( Prj (PrjL (PrjL (PrjR PrjZ))) x
           , Prj (PrjL (PrjR PrjZ)) x
           , Prj (PrjR PrjZ) x
           )

unMatch :: Exp a -> Exp a
unMatch (Match _ x) = x
unMatch x           = x

pattern T2 :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a,b)
pattern T2 x y = Pattern (x,y)
{-# COMPLETE T2 #-}

pattern T3 :: (Elt a, Elt b, Elt c) => Exp a -> Exp b -> Exp c -> Exp (a,b,c)
pattern T3 x y z = Pattern (x,y,z)
{-# COMPLETE T3 #-}

-- pattern T4 :: (Elt a, Elt b, Elt c, Elt d) => Exp a -> Exp b -> Exp c -> Exp d -> Exp (a,b,c,d)
-- pattern T4 x y z w = Pattern (x,y,z,w)
-- {-# COMPLETE T4 #-}

