{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Lambda where

import List
import Bool

import Match
import TH
import Elt
import Pattern
import Pretty                                             ()
import Exp                                                ( Exp )
import qualified Exp                                      as Exp

import Data.Char
import Data.Function


data Name = Name (List Int)
  deriving (Generic, Elt, IsTuple)

mkName :: String -> Name
mkName = Name . fromList . map ord

instance Show Name where
  show (Name l) = map chr (toList l)

data Lambda
  = Var Name
  | App Lambda Lambda
  | Lam Name Lambda
  deriving Show

data SKI
  = S
  | K
  | I
  | Var' Name
  | App' SKI SKI
  deriving Show

pattern Name_ :: Exp (List Int) -> Exp Name
pattern Name_ l = Pattern l
{-# COMPLETE Name_ #-}

mkAll ''Lambda
mkAll ''SKI

-- An expression language term that lifts expressions in lambda calculs to
-- expressions in SKI combinator calculus.
--
toSKI :: Exp Lambda -> Exp SKI
toSKI lam =
  let s = Exp.Idx "toSKI"
      r = Exp.Idx "remove"
      e = Exp.Idx "eqName"
      p = Exp.Idx "p"
      q = Exp.Idx "q"
      u = Exp.Idx "u"
      v = Exp.Idx "v"
      w = Exp.Idx "w"

      -- The auxiliary function 'remove' does the actual work...
      toSKI' = match \case
        Var_ x     -> Var'_ x
        App_ t1 t2 -> (Exp.Var s `Exp.App` t1) `App'_` (Exp.Var s `Exp.App` t2)
        Lam_ x t   -> Exp.Var r `Exp.App` x `Exp.App` (Exp.Var s `Exp.App` t)

      -- ...sometimes known as 'bracket abstraction'
      remove' x = match \case
        Var'_ y -> (Exp.Var e `Exp.App` x `Exp.App` y) & match \case
                     True_  -> I_
                     False_ -> K_ `App'_` Var'_ y
        App'_ t1 t2 -> S_ `App'_` (Exp.Var r `Exp.App` x `Exp.App` t1)
                         `App'_` (Exp.Var r `Exp.App` x `Exp.App` t2)
        y           -> K_ `App'_` y
  in
  Exp.Let e (Exp.Lam p (Exp.Lam q (eqName (Exp.Var p) (Exp.Var q)))) (
  Exp.Let r (Exp.Lam u (Exp.Lam v (remove' (Exp.Var u) (Exp.Var v)))) (
  Exp.Let s (Exp.Lam w (toSKI' (Exp.Var w))) (
    Exp.Var s `Exp.App` lam)))

eqName :: Exp Name -> Exp Name -> Exp Bool
eqName (Name_ x) (Name_ y) = eqList x y

eqList :: Exp (List Int) -> Exp (List Int) -> Exp Bool
eqList us vs =
  let l = Exp.Idx "eqList"
      u = Exp.Idx "u"
      v = Exp.Idx "v"

      go Nil_ Nil_                 = True_
      go (Cons_ x xs) (Cons_ y ys) = Exp.Eq x y & match \case
                                       False_ -> False_
                                       True_  -> Exp.Var l `Exp.App` xs `Exp.App` ys
      go _ _                       = False_
  in
  Exp.Let l (Exp.Lam u (Exp.Lam v ((match go) (Exp.Var u) (Exp.Var v)))) (Exp.Var l `Exp.App` us `Exp.App` vs)


liftLambda :: Lambda -> Exp Lambda
liftLambda (Var (Name n)) = Var_ (Name_ (liftList n))
liftLambda (App t1 t2) = App_ (liftLambda t1) (liftLambda t2)
liftLambda (Lam (Name n) t) = Lam_ (Name_ (liftList n)) (liftLambda t)

zero :: Lambda
zero = Lam (mkName "s") $ Lam (mkName "z") $ Var (mkName "z")

one :: Lambda
one  = Lam (mkName "s") $ Lam (mkName "z") $ App (Var (mkName "s")) (Var (mkName "z"))

suc :: Lambda
suc = Lam (mkName "n")
    $ Lam (mkName "s")
    $ Lam (mkName "z")
    $ App (Var (mkName "s")) (App (App (Var (mkName "n")) (Var (mkName "s"))) (Var (mkName "z")))

add :: Lambda
add = Lam (mkName "m")
    $ Lam (mkName "n")
    $ App (App (Var (mkName "m")) suc) (Var (mkName "n"))


test0 = toSKI (liftLambda zero)
test1 = toSKI (liftLambda one)

