{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PostfixOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test where

import Elt
import Exp
import List
import Match
import Maybe
import Point
import Pretty ()
import Rec

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
  let l    = Idx "length"
      v    = Idx "xs"
      fun  = Lam v (body (Var v))
      body = match \case
        Nil_        -> int 0
        Cons_ _ xs' -> int 1 `Add` App (Var l) xs'
  in
  Let l fun (App (Var l) xs)


int :: Int -> Exp Int
int = Constant

float :: Float -> Exp Float
float = Constant

tag :: TAG -> Exp TAG
tag = Constant

nil :: Exp (List Int)
nil = Tuple $ Exp (tag 0) `Pair` (Unit `Pair` Exp (Undef (eltR @Int))
                                       `Pair` Exp (Undef (eltR @(Rec (List Int)))))

infixr 5 `cons`
cons :: Elt a => Exp a -> Exp (List a) -> Exp (List a)
cons x xs = Tuple $ Exp (tag 1) `Pair` (Unit `Pair` Exp x `Pair` Exp (Roll xs))

pair :: Exp a -> Exp b -> Exp (a, b)
pair a b = Tuple $ Unit `Pair` Exp a `Pair` Exp b

fst :: Exp (a, b) -> Exp a
fst = Prj (PrjL (PrjR PrjZ))

snd :: Exp (a, b) -> Exp b
snd = Prj (PrjR PrjZ)

