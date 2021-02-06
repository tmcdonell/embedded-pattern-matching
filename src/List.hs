{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module List where

import Elt
import Rec
import Tuple
import Type
import Trace
import Exp


data List a = Nil | Cons a (List a)
  deriving Generic

instance Show a => Show (List a) where
  show = show . toList

toList :: List a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs

instance Elt a => Elt (List a) where
  type EltR (List a) = (TAG, EltR (a, Rec (List a)))

  fromElt Nil         = (0, (((), undefined), undefined))
  fromElt (Cons x xs) = (1, fromElt (x, Rec xs))

  toElt (0, _) = Nil
  toElt (1, l) = let (x, Rec xs) = toElt l
                  in Cons x xs
  toElt _      = error "internal error"

  eltR = eltR @TAG `TypeRpair` eltR @(a, Rec (List a))

  traceR = TraceRtag 0 (TraceRunit `TraceRpair` TraceRundef (eltR @a) `TraceRpair` TraceRundef (eltR @(Rec (List a))))
       : [ TraceRtag 1 (TraceRunit `TraceRpair` a `TraceRpair` TraceRrec (eltR @(List a))) | a <- traceR @a ]


instance IsTuple (List a) where
  type TupleR (List a) = (TAG, TupleR (a, Rec (List a)))
  fromTup Nil         = (0, (((), undefined), undefined))
  fromTup (Cons x xs) = (1, fromTup (x, Rec xs))

  toTup (0, _) = Nil
  toTup (1, t) = let (x, Rec xs) = toTup t
                  in Cons x xs
  toTup _      = error "internal error"


pattern Nil_ :: Elt a => Exp (List a)
pattern Nil_ <- (matchNil -> Just ())
  where Nil_ = buildNil

pattern Cons_ :: Elt a => Exp a -> Exp (List a) -> Exp (List a)
pattern Cons_ x xs <- (matchCons -> Just (x, xs))
  where Cons_ = buildCons
{-# COMPLETE Nil_, Cons_ #-}

buildNil :: forall a. Elt a => Exp (List a)
buildNil = Tuple $ Exp (Constant 0) `Pair` (Unit `Pair` Exp (Undef (eltR @a))
                                                 `Pair` Exp (Undef (eltR @(Rec (List a)))))

buildCons :: Elt a => Exp a -> Exp (List a) -> Exp (List a)
buildCons x xs = Tuple $ Exp (Constant 1) `Pair` (Unit `Pair` Exp x `Pair` Exp (Roll xs))

matchNil :: Elt a => Exp (List a) -> Maybe ()
matchNil (Match (TraceRtag 0 _) _) = Just ()
matchNil Match{} = Nothing
matchNil _       = error "matchNil: used outside 'match' context"

matchCons :: Elt a => Exp (List a) -> Maybe (Exp a, Exp (List a))
matchCons (Match (TraceRtag 1 (TraceRunit `TraceRpair` t `TraceRpair` TraceRrec _)) x)
  = Just ( Match t (Prj (PrjR (PrjL (PrjR PrjZ))) x)
         , Unroll (Prj (PrjR (PrjR PrjZ)) x)
         )
matchCons Match{} = Nothing
matchCons _       = error "matchCons: used outside 'match' context"

