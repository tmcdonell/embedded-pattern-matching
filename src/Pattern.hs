{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Pattern where

import Exp
import Elt
import Tag


class IsPattern s r where
  construct :: r -> Exp s
  destruct  :: Exp s -> r

pattern Pattern :: IsPattern s r => r -> Exp s
pattern Pattern vars <- (destruct -> vars)
  where Pattern = construct


-- XXX: The constraint on equality of representation types here is what
-- allows us to reuse this instance for all pair-like surface types. If we
-- don't have this, then for example:
--
-- > data V2 a = V2 a a
--
-- Will require
--
-- > instance IsPattern (V2 a) (Exp a, Exp a)
--
-- Which we don't want to have to provide.
--
-- For this to work it requires our internal (PreExp) terms to be in
-- representation format, and Exp to refer to the surface type.
--
instance EltR a ~ ((), EltR b) => IsPattern a (Exp b) where
  construct (Exp x) = Exp $ Unit `Pair` unMatch x
  destruct (Exp x) =
    case x of
      Match (TraceRunit `TraceRpair` a) x' -> Exp (Match a (PrjR x'))
      _ -> Exp (PrjR x)

instance (EltR r ~ EltR (a,b)) => IsPattern r (Exp a, Exp b) where
  construct (Exp x, Exp y) =
    Exp $ Unit `Pair` unMatch x `Pair` unMatch y
  destruct (Exp x) =
    case x of
      Match (TraceRunit `TraceRpair` a `TraceRpair` b) x'
        -> ( Exp (Match a (PrjR (PrjL x')))
           , Exp (Match b (PrjR x')))
      _ -> ( Exp (PrjR (PrjL x))
           , Exp (PrjR x))

instance (EltR r ~ EltR (a,b,c)) => IsPattern r (Exp a, Exp b, Exp c) where
  construct (Exp x, Exp y, Exp z) =
    Exp $ Unit `Pair` unMatch x `Pair` unMatch y `Pair` unMatch z
  destruct (Exp x) =
    case x of
      Match (TraceRunit `TraceRpair` a `TraceRpair` b `TraceRpair` c) x'
        -> ( Exp (Match a (PrjR (PrjL (PrjL x'))))
           , Exp (Match b (PrjR (PrjL x')))
           , Exp (Match c (PrjR x'))
           )
      _ -> ( Exp (PrjR (PrjL (PrjL x)))
           , Exp (PrjR (PrjL x))
           , Exp (PrjR x)
           )

instance (EltR r ~ EltR (a,b,c,d)) => IsPattern r (Exp a, Exp b, Exp c, Exp d) where
  construct (Exp x, Exp y, Exp z, Exp w) =
    Exp $ Unit `Pair` unMatch x `Pair` unMatch y `Pair` unMatch z `Pair` unMatch w
  destruct (Exp x) =
    case x of
      Match (TraceRunit `TraceRpair` a `TraceRpair` b `TraceRpair` c `TraceRpair` d) x'
        -> ( Exp (Match a (PrjR (PrjL (PrjL (PrjL x')))))
           , Exp (Match b (PrjR (PrjL (PrjL x'))))
           , Exp (Match c (PrjR (PrjL x')))
           , Exp (Match d (PrjR x'))
           )
      _ -> ( Exp (PrjR (PrjL (PrjL (PrjL x))))
           , Exp (PrjR (PrjL (PrjL x)))
           , Exp (PrjR (PrjL x))
           , Exp (PrjR x)
           )

-- XXX: Required to ensure that 'Match' nodes do not appear in the final
-- AST, as well as in the base instance of the Matching class. See comment
-- in Exp.hs
--
unMatch :: PreExp a -> PreExp a
unMatch (Match _ x) = x
unMatch x           = x

pattern T2 :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a,b)
pattern T2 x y = Pattern (x,y)
{-# COMPLETE T2 #-}

pattern T3 :: (Elt a, Elt b, Elt c) => Exp a -> Exp b -> Exp c -> Exp (a,b,c)
pattern T3 x y z = Pattern (x,y,z)
{-# COMPLETE T3 #-}

pattern T4 :: (Elt a, Elt b, Elt c, Elt d) => Exp a -> Exp b -> Exp c -> Exp d -> Exp (a,b,c,d)
pattern T4 x y z w = Pattern (x,y,z,w)
{-# COMPLETE T4 #-}

