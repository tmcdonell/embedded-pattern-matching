{-# LANGUAGE BlockArguments       #-}
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

module Mutual where

import Either
import Elt
import Exp
import Match
import Pretty ()
import TH

import Data.Function

data A = AZ | AA A | AB B
data B = BZ | BB B | BA A

mkAll' ''A [''B]
mkAll' ''B [''A]

dig :: Exp (Either A B) -> Exp Int
dig eab =
  let
      digA_ = Idx "digA"
      digB_ = Idx "digB"
      u     = Idx "u"
      v     = Idx "v"
      --
      digA = match \case
        AZ_   -> Const 0
        AA_ a -> App (Var digA_) a
        AB_ b -> App (Var digB_) b

      digB = match \case
        BZ_   -> Const 1
        BA_ a -> App (Var digA_) a
        BB_ b -> App (Var digB_) b
  in
  Let digA_ (Lam u (digA (Var u))) (
  Let digB_ (Lam v (digB (Var v))) (
    eab & match \case
      Left_  a -> App (Var digA_) a
      Right_ b -> App (Var digB_) b
  ))

t1 :: Exp Int
t1 = dig (Left_ AZ_)

