{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test where

import Bool
import Elt
import Exp
import List
import Match
import Maybe
import Point
import Pretty ()
import Rec
import Type

import Data.Function ( (&) )
import Prelude hiding ( fst, snd, length )


dummy :: forall a. Elt a => Exp a
dummy = Undef (eltR @a)

t1 :: Exp (List Int)
t1 = int 100 `cons` int 101 `cons` int 102 `cons` nil

t2 :: Exp (Maybe Int) -> Exp Int
t2 = match \case
  Nothing_ -> int 0
  Just_ x  -> x

t3 :: Exp (List Int) -> Exp Int
t3 = match \case
  Nil_       -> int 0
  Cons_ x xs -> xs & match \case
                  Nil_ -> int 1
                  _    -> x

t4 :: Exp (List Int)
t4 = Cons_ (int 0) Nil_

t5 :: Exp (Maybe (List Int)) -> Exp Int
t5 = match \case
  Nothing_           -> int 0
  Just_ Nil_         -> int 1
  Just_ (Cons_ x xs) -> xs & match \case
                          Nil_ -> int 2
                          _    -> x

t6 :: Exp (List Int) -> Exp Int
t6 = match \case
  Nil_         -> int 0
  Cons_ _ Nil_ -> int 1 -- XXX: does not work
  Cons_ _ _    -> int 2

t7 :: Exp (Maybe (Maybe Int)) -> Exp Int
t7 = match \case
  Nothing_ -> int 0
  Just_ Nothing_ -> int 1
  Just_ Just_{}  -> int 2

t8 :: Exp (List (Maybe Int)) -> Exp Int
t8 = match \case
  Nil_ -> int 0
  Cons_ Nothing_ _ -> int 1
  Cons_ Just_{} _ -> int 2

t9 :: Exp (List Point) -> Exp Float
t9 = match \case
  Nil_ -> float 0
  Cons_ (Point_ x _) _ -> x

t10 :: Exp (Maybe Bool) -> Exp Int
t10 = match \case
  Nothing_     -> int 0
  Just_ False_ -> int 1
  Just_ True_  -> int 2

safeHead :: Elt a => Exp (List a) -> Exp (Maybe a)
safeHead = match \case
  Nil_ -> Nothing_
  Cons_ x _ -> Just_ x

safeTail :: Elt a => Exp (List a) -> Exp (Maybe (List a))
safeTail = match \case
  Nil_ -> Nothing_
  Cons_ _ xs -> Just_ xs

-- length :: Elt a => Exp (List a) -> Exp Int
-- length = match \case
--   Nil_       -> int 0
--   Cons_ _ xs -> int 1 `Add` App f xs
--     where
--       ix = Idx "xs"
--       f  = Lam ix (length (Var ix))

length :: Elt a => Exp (List a) -> Exp Int
length xs =
  let body = match \case
        Nil_        -> int 0
        Cons_ _ xs' -> int 1 `Add` App (Var l) xs'
      --
      l    = Idx "length"
      v    = Idx "xs"
  in
  Let l (Lam v (body (Var v))) (App (Var l) xs)


-- constant :: Elt a => a -> Exp a
-- constant x = go eltR (toElt x)
--   where


int :: Int -> Exp Int
int = Const

float :: Float -> Exp Float
float = Const

word8 :: Word8 -> Exp Word8
word8 = Const

tag :: TAG -> Tuple TAG
tag = Exp . word8

liftList :: forall a. Elt a => List a -> Exp (List a)
liftList Nil = nil
liftList (Cons x xs) = Const (fromElt x) `cons` liftList xs

nil :: forall a. Elt a => Exp (List a)
nil = Tuple $ tag 0 `Pair` (Unit `Pair` Exp (Undef (eltR @a))
                                 `Pair` Exp (Undef (eltR @(Rec (List a)))))

infixr 5 `cons`
cons :: Elt a => Exp a -> Exp (List a) -> Exp (List a)
cons x xs = Tuple $ tag 1 `Pair` (Unit `Pair` Exp x `Pair` Exp (Roll xs))

pair :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a, b)
pair a b = Tuple $ Unit `Pair` Exp a `Pair` Exp b

fst :: (Elt a, Elt b) => Exp (a, b) -> Exp a
fst = Prj (PrjL (PrjR PrjZ))

snd :: (Elt a, Elt b) => Exp (a, b) -> Exp b
snd = Prj (PrjR PrjZ)

