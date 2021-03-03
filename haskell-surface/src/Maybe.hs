{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Maybe (
  Maybe(..), liftMaybe,
  pattern Nothing_,
  pattern Just_,
) where

import Elt
import Exp
import Trace
import Tuple

instance Elt a => Elt (Maybe a) where
{--
  type EltR (Maybe a) = (TAG, EltR a)

  fromElt Nothing  = (0, undefined)
  fromElt (Just x) = (1, fromElt x)

  toElt (0, _) = Nothing
  toElt (1, x) = Just (toElt x)
  toElt _      = error "internal error"

  eltR = eltR @TAG `TypeRpair` eltR @a

  traceR = TraceRtag 0 (TraceRundef (eltR @a))
       : [ TraceRtag 1 a | a <- traceR @a ]
--}

-- type instance TupleR (Maybe a) = (TAG, a)

instance IsTuple (Maybe a) where
  -- type TupleR (Maybe a) = (TAG, a)

  fromTup Nothing  = (0, ((), undefined))
  fromTup (Just x) = (1, ((), x))

  toTup (0, ((), _)) = Nothing
  toTup (1, ((), x)) = Just x
  toTup _            = error "internal error"

pattern Nothing_ :: Elt a => Exp (Maybe a)
pattern Nothing_ <- (matchNothing -> Just ())
  where Nothing_ = buildNothing

pattern Just_ :: Elt a => Exp a -> Exp (Maybe a)
pattern Just_ x <- (matchJust -> Just x)
  where Just_ = buildJust
{-# COMPLETE Nothing_, Just_ #-}

tag :: TAG -> Tuple TAG
tag x = Exp $ Const x

liftMaybe :: forall a. Elt a => Maybe a -> Exp (Maybe a)
liftMaybe Nothing  = Tuple $ tag 0 `Pair` (Unit `Pair` Exp (Undef (eltR @a)))
liftMaybe (Just a) = Tuple $ tag 1 `Pair` (Unit `Pair` Exp (Const (fromElt a)))

buildNothing :: forall a. Elt a => Exp (Maybe a)
buildNothing = Tuple $ tag 0 `Pair` (Unit `Pair` Exp (Undef (eltR @a)))

buildJust :: Elt a => Exp a -> Exp (Maybe a)
buildJust x = Tuple $ tag 1 `Pair` (Unit `Pair` Exp x)

matchNothing :: Exp (Maybe a) -> Maybe ()
matchNothing (Match (TraceRtag 0 _) _) = Just ()
matchNothing Match{} = Nothing
matchNothing _       = error "matchNothing: used outside 'match' context"

matchJust :: Exp (Maybe a) -> Maybe (Exp a)
matchJust (Match (TraceRtag 1 (TraceRunit `TraceRpair` t)) x) = Just $ Match t (Prj (PrjR (PrjR PrjZ)) x)
matchJust Match{} = Nothing
matchJust _       = error "matchJust: used outside 'match' context"

