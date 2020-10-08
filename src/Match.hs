{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Match where

import Exp
import Elt
import qualified Debug

import Text.Printf


-- Embedded pattern matching
-- =========================

match :: Matching f => f -> f
match f = mkFun (mkMatch f) id

data Args f where
  (:->)  :: Exp a -> Args b -> Args (Exp a -> b)
  Result :: Args (Exp a)

class Matching a where
  type ResultT a
  mkMatch :: a -> Args a -> Exp (ResultT a)
  mkFun   :: (Args f -> Exp (ResultT a))
          -> (Args a -> Args f)
          -> a

instance Elt a => Matching (Exp a) where
  type ResultT (Exp a) = a

  mkFun f k = f (k Result)

  -- XXX: We can add this to remove any trailing 'Match' constructs.
  -- Removing them here, and in Pattern.hs when constructing tuples, is
  -- enough to make sure Match does not appear in the final AST. We don't
  -- mention this in the paper, but it is a straightforward consequence of
  -- propogating sub-pattern matches which are then matched against
  -- variables/wildcard which don't activate the embedded pattern match and
  -- thus do not consume the 'Match' constructor. For example:
  --
  -- > f :: Exp (Maybe Bool) -> Exp Int
  -- > f = match \case
  -- >   Nothing_ ->
  -- >   Just_ _  ->
  --
  mkMatch (Exp e) Result = case e of
                             Match _ x -> Exp x
                             _         -> Exp e

instance (Elt e, Matching r) => Matching (Exp e -> r) where
  type ResultT (Exp e -> r) = ResultT r

  mkFun f k = \x -> mkFun f (\xs -> k (x :-> xs))

  mkMatch f (x@(Exp p) :-> xs) =
    case p of
      -- XXX: This first case is used when we have nested calls to 'match',
      -- such as in Test.t8, and removes the redundant 'Match' terms
      --
      Match{} -> mkMatch (f x) xs

      -- XXX: If there is only a single alternative, we can elide the case
      -- statement at this point. This can occur when pattern matching on
      -- product types, e.g. Test.t13
      --
      -- > t13 :: Exp (Int,Int) -> Exp (Int,Int,Int,Int)
      -- > t13 (T2 x y) = T4 x x y y
      --
      -- Goes from:
      --
      -- > case tag# undef of
      -- >  0.0.0# -> ( prjR (prjL (undef))
      -- >            , prjR (prjL (undef))
      -- >            , prjR (undef)
      -- >            , prjR (undef) )
      --
      -- to:
      --
      -- > (prjR (prjL (undef)), prjR (prjL (undef)), prjR (undef), prjR (undef))
      --
      -- Which is equivalent to not using the `match` operator at all.
      --
      _       -> case rhs of
                   [(_,r)] -> Exp $ r
                   _       -> Exp $ Case p rhs
    where
      rhs = [ Debug.trace (printf "mkMatch: trace=%s" (show trace))
            $ (trace, unExp (mkMatch (f x') xs))
            | trace <- traceR @e
            , let x' = Exp (Match trace p) ]

      -- XXX TODO: Generate nested cases.
      --
      -- Because we must enumerate the entire pathway through a type, to
      -- explore every possible pattern match, we get a flat structure. For
      -- example, these are the tags for the type 'Maybe (Bool,Bool)'
      --
      -- > [TagRtag (GHC.Word.W8# 0) (TagRpair TagRunit (TagRpair (TagRpair TagRunit (TagRpair TagRundef TagRunit)) (TagRpair TagRundef TagRunit)))
      -- > ,TagRtag (GHC.Word.W8# 1) (TagRpair TagRunit (TagRpair (TagRpair TagRunit (TagRtag (GHC.Word.W8# 0) TagRunit)) (TagRtag (GHC.Word.W8# 0) TagRunit)))
      -- > ,TagRtag (GHC.Word.W8# 1) (TagRpair TagRunit (TagRpair (TagRpair TagRunit (TagRtag (GHC.Word.W8# 0) TagRunit)) (TagRtag (GHC.Word.W8# 1) TagRunit)))
      -- > ,TagRtag (GHC.Word.W8# 1) (TagRpair TagRunit (TagRpair (TagRpair TagRunit (TagRtag (GHC.Word.W8# 1) TagRunit)) (TagRtag (GHC.Word.W8# 0) TagRunit)))
      -- > ,TagRtag (GHC.Word.W8# 1) (TagRpair TagRunit (TagRpair (TagRpair TagRunit (TagRtag (GHC.Word.W8# 1) TagRunit)) (TagRtag (GHC.Word.W8# 1) TagRunit)))]
      --
      -- When generating the 'Case' terms we could look over this structure
      -- and group alternatives which have the same initial tag. For
      -- constructors with more than one field this is a bit more
      -- difficult, but still doable. For the following:
      --
      -- > t23 :: Exp (Maybe (Bool,Bool)) -> Exp Int
      -- > t23 x = x & match \case
      -- >   Nothing_                 -> constant 0
      -- >   Just_ (T2 False_ False_) -> constant 1
      -- >   Just_ (T2 False_ True_)  -> constant 2
      -- >   Just_ (T2 True_  False_) -> constant 3
      -- >   Just_ (T2 True_  True_)  -> constant 4
      --
      -- Instead of:
      --
      -- > case tag# undef of
      -- >   0....# -> 0
      -- >   1.0.0# -> 1
      -- >   1.0.1# -> 2
      -- >   1.1.0# -> 3
      -- >   1.1.1# -> 4
      --
      -- We should generate something like:
      --
      -- > case tag# undef of
      -- >   0._# -> 0
      -- >   1._# -> case tag# of
      -- >             _.0._# -> case tag# of
      -- >                         _._.0# -> 1
      -- >                         _._.1# -> 2
      -- >             _.1._# -> case tag# of
      -- >                         _._.0# -> 3
      -- >                         _._.1# -> 4
      --

