{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Example where

import Elt
import Exp
import Match
import Pattern
import TH
import Bool
import Pretty () -- show instance

import Data.Function


-- Section 5.1
-- ===========

data Point = Point Float Float
  deriving (Generic, Elt)

pattern Point_ :: Exp Float -> Exp Float -> Exp Point
pattern Point_ x y = Pattern (x, y)
{-# COMPLETE Point_ #-}

liftPoint :: Point -> Exp Point
liftPoint (Point x y) = Point_ (constant x) (constant y)

xcoord :: Exp Point -> Exp Float
xcoord (Point_ x _) = x

-- We didn't define (+) for our minimal language!
--
-- addPoint :: Exp Point -> Exp Point -> Exp Point
-- addPoint (Point_ x1 y1) (Point_ x2 y2) = Point_ (x1+x2) (y1+y2)


-- Section 5.2
-- ===========

data Option a = None | Some a
  deriving (Generic, Elt)

mkPattern ''Option

simple :: Exp (Option Int) -> Exp Int
simple None_     = constant 0
simple (Some_ x) = x

nested2 :: Exp (Option Int, Bool) -> Exp Int
nested2 x = x & match \case
  T2 None_ False_     -> constant 0
  T2 None_ True_      -> constant 1
  T2 (Some_ _) False_ -> constant 2
  T2 (Some_ y) True_  -> y

