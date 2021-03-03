{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE InstanceSigs          #-}

module Exp where

import Elt
import Tag
import Type

import Data.Dynamic
import Data.Map                                           ( Map )
import Data.Text                                          ( Text )
import qualified Data.Map                                 as Map
import qualified Data.Text                                as T


-- Expression language
-- ===================

-- XXX: This is required. The core language is in representation format,
-- and the user-facing Exp is the only thing which refers to the surface
-- type.
--
-- See also the IsPattern.
--
data Exp a = Exp { unExp :: PreExp (EltR a) }

data PreExp a where
  -- This is the core language
  --
  Constant  :: PrimType a -> a -> PreExp a
  Pair      :: PreExp a -> PreExp b -> PreExp (a,b)
  Unit      :: PreExp ()
  PrjL      :: PreExp (a,b) -> PreExp a
  PrjR      :: PreExp (a,b) -> PreExp b

  -- Constructs to support pattern matching. The Undef case is not strictly
  -- necessary; we could just use Constant terms instead.
  --
  Undef     :: TypeR a -> PreExp a
  Match     :: TraceR a -> PreExp a -> PreExp a
  Case      :: PreExp a -> [(TraceR a, PreExp b)] -> PreExp b

  -- Added variables and let bindings, but actually it wasn't used
  Var       :: Idx t -> PreExp t
  Let       :: Idx a -> PreExp a -> PreExp b -> PreExp b


pattern Tag :: TAG -> PreExp TAG
pattern Tag x = Constant (IntegralNumType TypeWord8) x

data Idx t where
  Idx :: TypeR t -> Text -> Idx t

type Env = Map Text Dynamic

lookupEnv :: forall t. Idx t -> Env -> t
lookupEnv (Idx t nm) env
  | TypeableDict <- typeableDict t
  , Just v       <- Map.lookup nm env
  , Just v'      <- fromDynamic v
  = v'
  | otherwise
  = error ("lookupEnv: not found: " ++ T.unpack nm)

eval :: Elt a => Exp a -> a
eval = toElt . eval' Map.empty . unExp

eval' :: forall a. Env -> PreExp a -> a
eval' env = \case
  Constant _ x  -> x
  Pair l r      -> (eval' env l, eval' env r)
  Unit          -> ()
  (PrjL x)      -> let (l,_) = eval' env x in l
  (PrjR x)      -> let (_,r) = eval' env x in r
  --
  Var ix            -> lookupEnv ix env
  Let (Idx t v) a b
    | TypeableDict <- typeableDict t
    -> eval' (Map.insert v (toDyn (eval' env a)) env) b
  --
  Undef t       -> undef t
  Match{}       -> error "eval': match"
  Case x xs     -> eval' env $ lookupCase (eval' env x) xs

constant :: forall a. Elt a => a -> Exp a
constant v = Exp $ constant' (eltR @a) (fromElt v)

constant' :: TypeR t -> t -> PreExp t
constant' TypeRunit         ()    = Unit
constant' (TypeRprim t)     a     = Constant t a
constant' (TypeRpair ta tb) (a,b) = constant' ta a `Pair` constant' tb b

eqTrace :: TraceR a -> a -> Bool
eqTrace TraceRunit         ()     = True
eqTrace (TraceRprim _)     _      = True
eqTrace (TraceRundef _)    _      = True
eqTrace (TraceRtag tag ta) (t, a) = t == tag && eqTrace ta a
eqTrace (TraceRpair ta tb) (a, b) = eqTrace ta a && eqTrace tb b

lookupCase :: a -> [(TraceR a, PreExp b)] -> PreExp b
lookupCase val = go
  where
    go [] = error "lookupCase: unmatched case"
    go ((tag,cont):rest)
      | eqTrace tag val = cont
      | otherwise       = go rest

