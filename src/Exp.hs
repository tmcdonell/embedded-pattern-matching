{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exp where

import Elt
import Rec
import Trace
import Tuple

import Data.Dynamic
import Data.Map                                           ( Map )
import Data.Text                                          ( Text )
import qualified Data.Map                                 as Map
import qualified Data.Text                                as T


data Exp a where
  Constant :: Elt a => EltR a -> Exp a

  Unroll   :: Exp (Rec a) -> Exp a
  Roll     :: Exp a -> Exp (Rec a)

  Tuple    :: IsTuple t => Tuple (TupleR t) -> Exp t
  Prj      :: IsTuple t => TupleIdx (TupleR t) e -> Exp t -> Exp e

  -- Tuple    :: Exp (TupleR a) -> Exp a
  -- Unit     :: Exp ()
  -- Pair     :: Exp a -> Exp b -> Exp (a, b)

  -- PrjL     :: Exp (a, b) -> Exp a
  -- PrjR     :: Exp (a, b) -> Exp b

  Undef     :: Elt a => Exp a
  Match     :: TraceR (EltR a) -> Exp a -> Exp a
  Case      :: Elt a => Exp a -> [(TraceR (EltR a), Exp b)] -> Exp b

  Var       :: Idx t -> Exp t
  Let       :: Idx a -> Exp a -> Exp b -> Exp b

type Name = Text

data Idx t where
  Idx :: Elt t => Name -> Idx t

data Tuple t where
  Unit :: Tuple ()
  Exp  :: Exp a -> Tuple a
  Pair :: Tuple a -> Tuple b -> Tuple (a, b)

-- data Tuple t where
--   NilTup   :: Tuple ()
--   TagTup   :: TAG -> Tuple TAG
--   SnocTup  :: Elt t => Tuple s -> Exp t -> Tuple (s, t)

-- infixl 1 !
-- (!) :: Exp a -> Exp (Rec a)
-- (!) = Roll

-- ref :: Exp a -> Exp (Rec a)
-- ref = Roll

-- deref :: Exp (Rec a) -> Exp a
-- deref = Unroll

-- infixl 1 !
-- (!) :: Exp (Rec a) -> Exp a
-- (!) = Unroll



type Env = Map Name Dynamic

eval :: Exp a -> a
eval = evalExp Map.empty

evalExp :: Env -> Exp a -> a
evalExp env = \case
  Constant c  -> toElt c
  Unroll e    -> let Rec x = evalExp env e in x
  Roll e      -> Rec (evalExp env e)
  Tuple t     -> toTup $ evalTup env t
  Prj tix t   -> evalPrj tix (fromTup (evalExp env t))
  Undef{}     -> error "evalExp: undef"
  Match{}     -> error "evalExp: match"
  Case x xs   -> evalExp env $ lookupCase (fromElt (evalExp env x)) xs
  Var ix      -> lookupEnv ix env
  Let (Idx v) a b
    -> evalExp (Map.insert v (toDyn (evalExp env a)) env) b

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

