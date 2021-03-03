{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
-- {-# OPTIONS_GHC -ddump-splices               #-}

module Test where

-- Example expression language
import Elt                                                hiding ( undef )
import Exp
import Match
import Pattern
import Pretty
import TH
import Type

-- Example data types
import Bool
import Either
import Maybe

-- Standard libraries
import Data.Word
import Data.Function
import Data.Typeable
import Data.Tree
import GHC.Generics
import Text.Printf
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.IO                             as T
import qualified Data.Map                                 as Map


data Test where
  T :: Exp a -> Test

test_exp :: IO ()
test_exp = mapM_ (uncurry do_test)
  [ ("t1",  T t1)
  , ("t2",  T t2)
  , ("t3",  T t3)
  , ("t4",  T t4)
  , ("t5",  T t5)
  , ("t6",  T t6)
  , ("t7",  T t7)
  , ("t8",  T t8)
  , ("t9",  T (match t9 undef))
  , ("t10", T (match t10 undef))
  , ("t11", T (match t11 undef undef))
  , ("t12", T (match t12 undef))
  , ("t13", T (match t13 undef))
  , ("t14", T (match t14 undef))
  , ("t15a", T (t15 undef))
  , ("t15b", T (match t15 undef))
  , ("t16", T (match t16 undef))
  , ("t17", T (match t17 Nothing_ (constant 42) (Right_ (constant 12))))
  , ("t20", T (t20 undef))
  , ("t21", T t21)
  , ("t23", T (t23 undef))
  , ("t24", T (t24 undef))
  ]
  where
    do_test :: String -> Test -> IO ()
    do_test nm (T e) = do
      T.putStrLn
        . renderStrict
        . layoutSmart defaultLayoutOptions
        . annotate (colorDull Green)
        $ "==" <+> pretty nm <+> pretty (replicate (86-length nm) '=')
      putStrLn (show e)
      putStrLn ""

test_mask :: IO ()
test_mask = sequence_
  [ do_test @Int
  , do_test @Bool
  , do_test @(Int,Int)
  , do_test @((Int,Int),(Int,Int))
  , do_test @(Int,Bool)
  , do_test @(Bool,Bool)
  , do_test @(Int, (Bool,Bool))
  , do_test @(Maybe Int)
  , do_test @(Maybe (Maybe Int))
  , do_test @Hex
  ]
  where
    do_test :: forall e. (Typeable e, Elt e) => IO ()
    do_test = do
      let nm = showsTypeRep (typeRep (Proxy :: Proxy e)) ""
      T.putStrLn
        . renderStrict
        . layoutSmart defaultLayoutOptions
        . annotate (colorDull Green)
        $ "==" <+> pretty nm <+> pretty (replicate (86-length nm) '=')
      putStrLn . unlines . map show $ traceR @e

data V2 a = V2 a a
  deriving (Show, Generic, Elt)

pattern V2_ :: Elt a => Exp a -> Exp a -> Exp (V2 a)
pattern V2_ x y = Pattern (x,y)
{-# COMPLETE V2_ #-}

data Point = Point Float Float
  deriving (Show, Generic, Elt)

pattern Point_ :: Exp Float -> Exp Float -> Exp Point
pattern Point_ x y = Pattern (x,y)
{-# COMPLETE Point_ #-}

data Hex = X Int | A | B | C | D | E | F
  deriving (Show, Generic, Elt)


undef :: forall a. Elt a => Exp a
undef = Exp (Undef (eltR @a))

t1 :: Exp (Int, Float)
t1 = T2 (constant 42) (constant pi)

t2 :: Exp Point
t2 = Point_ (constant 0) (constant 42)

t3 :: Exp (Either Int Float)
t3 = Left_ (constant 12)

t4 :: Exp Bool
t4 = match isJust (Just_ (constant pi :: Exp Float))

isJust :: Elt a => Exp (Maybe a) -> Exp Bool
isJust Nothing_ = constant False
isJust Just_{}  = constant True

isNothing :: Elt a => Exp (Maybe a) -> Exp Bool
isNothing Nothing_ = True_
isNothing Just_{}  = False_

t5 :: Exp (Int,Float)
t5 =
  let x0 = Idx (eltR @Int)   "x0"
      x1 = Idx (eltR @Float) "x1"
  in
  Exp
   $ Let x0 (constant' (eltR @Int)   0)
   $ Let x1 (constant' (eltR @Float) pi)
            (Unit `Pair` Var x0 `Pair` Var x1)

t6 :: Exp Bool
t6 =
  let v = Idx (eltR @(Maybe Point)) "v"
   in Exp
    $ Let v (unExp $ Just_ t2)
            (unExp $ match isJust (Exp (Var v) :: Exp (Maybe Point)))

t7 :: Exp Int
t7 = match t7' undef

t7' :: Exp (Either (Maybe Bool) (Maybe (Maybe Int))) -> Exp Int
t7' (Left_ Nothing_)          = constant 0
t7' (Left_ (Just_ True_))     = constant 1
t7' (Left_ (Just_ False_))    = constant 2
t7' (Right_ Nothing_)         = constant 3
t7' (Right_ (Just_ Nothing_)) = constant 4
t7' (Right_ (Just_ Just_{}))  = constant 5
-- t7' _ = constant (-1)

-- XXX: Nested 'match' calls don't lead to a blowup in the intermediate AST
--
t8 :: Exp Int
t8 = t8_1 (Just_ (Just_ (constant 42)))

t8_1 :: Exp (Maybe (Maybe Int)) -> Exp Int
t8_1 = match go
  where
    go Nothing_  = constant 0
    go (Just_ x) = t8_2 x
    -- go _         = constant (-1)

t8_2 :: Exp (Maybe Int) -> Exp Int
t8_2 = match go
  where
    go Nothing_  = constant 1
    go (Just_ x) = x
    -- go _         = constant (-2)

t9 :: Exp (Maybe Int, Int) -> Exp Int
t9 (T2 Nothing_  x) = x
t9 (T2 (Just_ x) _) = x
-- t9 _                = constant 0

t10 :: Exp (Int, Int) -> Exp Int
t10 (T2 x _) = x
-- t10 _        = constant 1

-- XXX: Note that the patterns appear in the tag order, not in the order
-- they are written in the concrete syntax
--
t11 :: Exp (Maybe Int) -> Exp (Either Int Int) -> Exp Int
t11 Just_{}  Left_{}  = constant 3
t11 Just_{}  Right_{} = constant 4
t11 Nothing_ Left_{}  = constant 1
t11 Nothing_ Right_{} = constant 2

t12 :: Exp (Maybe Int, Bool) -> Exp Int
t12 (T2 Nothing_  False_) = constant 0
t12 (T2 Nothing_  True_)  = constant 1
t12 (T2 (Just_ _) _)      = constant 2

fromBool :: Exp Bool -> Exp Int
fromBool True_  = constant 1
fromBool False_ = constant 0

t13 :: Exp (Int,Int) -> Exp (Int,Int,Int,Int)
t13 (T2 x y) = T4 x x y y

t14 :: Exp ((Int,Int), Bool) -> Exp Int
t14 (T2 (T2 x _) True_)  = x
t14 (T2 _        False_) = constant 0

t15 :: Exp ((Int,Int), (Int,Int)) -> Exp Int
t15 (T2 _ (T2 _ x)) = x

t16 :: Exp (V2 (Maybe (Maybe Int))) -> Exp Int
t16 (V2_ Nothing_          Nothing_)          = constant 0
t16 (V2_ Nothing_          (Just_ Nothing_))  = constant 1
t16 (V2_ Nothing_          (Just_ (Just_ y))) = y
t16 (V2_ (Just_ Nothing_)  Nothing_)          = constant 3
t16 (V2_ (Just_ (Just_ x)) Nothing_)          = x
t16 (V2_ Just_{}           Just_{})           = constant 5

t17 :: Exp (Maybe Int) -> Exp Int -> Exp (Either () Int) -> Exp (Int, Int)
t17 Nothing_ x (Left_ _)  = T2 x (constant 0)
t17 Nothing_ x (Right_ y) = T2 x y
t17 Just_{}  _ _          = T2 (constant 0) (constant 1)

t20 :: Exp (Maybe Bool, Bool) -> Exp Int
t20 = match $ \case
  T2 Nothing_       False_ -> constant 0
  T2 Nothing_       True_  -> constant 1
  T2 (Just_ False_) False_ -> constant 2
  T2 (Just_ False_) True_  -> constant 3
  T2 (Just_ True_)  False_ -> constant 4
  T2 (Just_ True_)  True_  -> constant 5

t21 :: Exp Bool
t21 =
  let x = Just_ (constant @Int 42)
   in x & match \case
        Just_{}  -> True_
        Nothing_ -> False_

t23 :: Exp (Maybe (Bool,Bool)) -> Exp Int
t23 x = x & match \case
  Nothing_                 -> constant 0
  Just_ (T2 False_ False_) -> constant 1
  Just_ (T2 False_ True_)  -> constant 2
  Just_ (T2 True_  False_) -> constant 3
  Just_ (T2 True_  True_)  -> constant 4

t24 :: Exp (Maybe (Bool,Bool)) -> Exp Int
t24 x = x & match \case
  Nothing_ -> constant 0
  Just_ _  -> constant 1

t26 :: Exp (Maybe (Maybe Int)) -> Exp (Maybe Int)
t26 x = x & match \case
              Nothing_ -> Nothing_
              Just_ y  -> y


data V3 a = V3 !a !a !a
  deriving (Show, Generic, Elt)

newtype Zq = Zq Int
  deriving (Show, Generic, Elt)

data Option a = None | Some a
  deriving (Show, Generic, Elt)

mkPattern ''Option
mkPattern ''V3
mkPattern ''Zq

