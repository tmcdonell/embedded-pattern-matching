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
instance Elt a => IsTuple (Maybe a) where


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

matchNothing :: Elt a => Exp (Maybe a) -> Maybe ()
matchNothing (Match (TraceRtag 0 _) _) = Just ()
matchNothing Match{} = Nothing
matchNothing _       = error "matchNothing: used outside 'match' context"

matchJust :: Elt a => Exp (Maybe a) -> Maybe (Exp a)
matchJust (Match (TraceRtag 1 (TraceRunit `TraceRpair` t)) x) = Just $ Match t (Prj (PrjR (PrjR PrjZ)) x)
matchJust Match{} = Nothing
matchJust _       = error "matchJust: used outside 'match' context"

