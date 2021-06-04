{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE TemplateHaskell      #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module List (

  List(..), toList, fromList, liftList,
  pattern Nil_,
  pattern Cons_,

) where

import TH
import Exp

data List a = Nil | Cons a (List a)

instance Show a => Show (List a) where
  show = show . toList

toList :: List a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs

fromList :: [a] -> List a
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)

mkAll ''List

liftList :: Elt a => List a -> Exp (List a)
liftList Nil = Nil_
liftList (Cons x xs) = Cons_ (Const (fromElt x)) (liftList xs)

