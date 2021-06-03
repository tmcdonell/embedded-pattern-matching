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

module Meta where

import Exp
import Match
import TH
import Pretty ()

data NExp
    = Number Int
    | NAdd NExp NExp

mkAll ''NExp

neval :: Exp NExp -> Exp Int
neval ne =
  let f = Idx "neval"
      v = Idx "v"
      --
      body = match \case
        Number_ x -> x
        NAdd_ x y -> App (Var f) x `Add` App (Var f) y
  in
  Let f (Lam v (body (Var v))) (App (Var f) ne)


fib :: Int -> Exp NExp
fib 0 = Number_ (Const 0)
fib 1 = Number_ (Const 1)
fib n = NAdd_ (fib (n-1)) (fib (n-2))

test1 :: Exp Int
test1 = neval (fib 12)

test2 :: Int
test2 = eval test1

