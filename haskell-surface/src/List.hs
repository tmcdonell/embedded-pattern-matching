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

  List(..), toList, fromList,
  pattern Nil_,
  pattern Cons_,

) where

import TH

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

