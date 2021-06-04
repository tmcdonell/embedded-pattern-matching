{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exp where

import Elt
import Rec
import Trace
import Tuple
import Type
import Debug

import Data.Dynamic
import Data.Map                                           ( Map )
import Data.Text                                          ( Text )
import qualified Data.Map                                 as Map
import qualified Data.Text                                as T


data Exp a where
  -- STLC with explicit let-binding. We don't bother with the type-level environment.
  -- Values are stored in representation format
  Const     :: Elt a => EltR a -> Exp a
  Var       :: Idx t -> Exp t
  Let       :: Idx a -> Exp a -> Exp b -> Exp b -- letrec
  App       :: Exp (a -> b) -> Exp a -> Exp b
  Lam       :: Idx a -> Exp b -> Exp (a -> b)

  -- Add generic product types
  Tuple     :: IsTuple t => Tuple (TupleR t) -> Exp t
  Prj       :: IsTuple t => TupleIdx (TupleR t) e -> Exp t -> Exp e

  -- Recursive types
  Unroll    :: Exp (Rec a) -> Exp a
  Roll      :: Exp a -> Exp (Rec a)

  -- Pattern matching
  Undef     :: TypeR (EltR a) -> Exp a
  Match     :: TraceR (EltR a) -> Exp a -> Exp a
  Case      :: Elt a => Exp a -> [(TraceR (EltR a), Exp b)] -> Exp b

  -- PrimOps
  Add       :: Exp Int -> Exp Int -> Exp Int
  Eq        :: Exp Int -> Exp Int -> Exp Bool

-- Tuples are heterogeneous lists using () and (,)
--
data Tuple t where
  Unit :: Tuple ()
  Exp  :: Exp a -> Tuple a
  Pair :: Tuple a -> Tuple b -> Tuple (a, b)

data TupleIdx s t where
  PrjZ ::                 TupleIdx t      t
  PrjL :: TupleIdx l t -> TupleIdx (l, r) t
  PrjR :: TupleIdx r t -> TupleIdx (l, r) t


-- Very unsafe variable bindings!
--
data Idx t where
  Idx :: Typeable t => Text -> Idx t

type Env = Map Text Dynamic


-- Standard type-safe evaluator
--
eval :: Exp a -> a
eval = evalExp Map.empty

evalExp :: Env -> Exp a -> a
evalExp env = \case
  Const c         -> toElt c
  Var ix          -> lookupEnv ix env
  Let (Idx v) a b -> let env' = Map.insert v (toDyn (evalExp env' a)) env
                      in evalExp env' b
  Lam (Idx v) b   -> \a -> evalExp (Map.insert v (toDyn a) env) b
  App f x         -> (evalExp env f) (evalExp env x)
  Tuple t         -> toTup $ evalTup env t
  Prj tix t       -> evalPrj tix (fromTup (evalExp env t))
  Unroll e        -> let Rec x = evalExp env e in x
  Roll e          -> Rec (evalExp env e)
  Undef{}         -> error "evalExp: undef"
  Match _ e       -> if dEBUG
                       then error "evalExp: match"
                       else evalExp env e
  Case x xs       -> evalExp env $ lookupCase (fromElt (evalExp env x)) xs
  --
  Add x y         -> evalExp env x + evalExp env y
  Eq x y          -> evalExp env x == evalExp env y

evalTup :: Env -> Tuple t -> t
evalTup env = \case
  Unit     -> ()
  Pair a b -> (evalTup env a, evalTup env b)
  Exp e    -> evalExp env e

evalPrj :: TupleIdx t e -> t -> e
evalPrj PrjZ e = e
evalPrj (PrjL t) (l, _) = evalPrj t l
evalPrj (PrjR t) (_, r) = evalPrj t r

lookupCase :: a -> [(TraceR a, Exp b)] -> Exp b
lookupCase val = go
  where
    go [] = error "lookupCase: unmatched case"
    go ((tag,cont):rest)
      | eqTrace tag val = cont
      | otherwise       = go rest

lookupEnv :: forall t. Idx t -> Env -> t
lookupEnv (Idx nm) env
  | Just v  <- Map.lookup nm env
  , Just v' <- fromDynamic v
  = v'
  | otherwise
  = error ("lookupEnv: not found: " ++ T.unpack nm)

eqTrace :: TraceR a -> a -> Bool
eqTrace TraceRunit         ()      = True
eqTrace (TraceRrec _)      (Rec _) = True
eqTrace (TraceRprim _)     _       = True
eqTrace (TraceRundef _)    _       = True
eqTrace (TraceRtag tag ta) (t, a)  = t == tag && eqTrace ta a
eqTrace (TraceRpair ta tb) (a, b)  = eqTrace ta a && eqTrace tb b

