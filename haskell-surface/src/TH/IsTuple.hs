{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module TH.IsTuple (
  mkIsTuple,
  mkIsTupleR
) where

import Elt
import Exp
import Rec
import Trace
import Tuple
import Type
import TH.Common

import Data.Bits
import Data.List
import Control.Monad
import Language.Haskell.TH                        hiding ( Exp, Match, match )
import qualified Language.Haskell.TH              as TH


mkIsTuple :: Name -> DecsQ
mkIsTuple nm = mkIsTupleR nm []

mkIsTupleR :: Name -> [Name] -> DecsQ
mkIsTupleR nm pts = do
  info <- reify nm
  case info of
    TyConI dec -> mkDec pts dec
    _          -> fail "mkIsTuple: expected the name of a newtype or datatype"

mkDec :: [Name] -> Dec -> DecsQ
mkDec pts dec =
  case dec of
    DataD    _ nm tv _ cs _ -> mkDataD nm (nm:pts) tv cs
    NewtypeD _ nm tv _ c  _ -> mkNewtypeD nm pts tv c
    _                       -> fail "mkIsTuple: expected the name of a newtype or datatype"

mkNewtypeD :: Name -> [Name] -> [TyVarBndr] -> Con -> DecsQ
mkNewtypeD nm pts tvbs c = mkDataD nm (nm:pts) tvbs [c]

mkDataD :: Name -> [Name] -> [TyVarBndr] -> [Con] -> DecsQ
mkDataD nm pts tvbs cs = do
  (from,to) <- unzip <$> go [] fts cs cts
  dec       <- isTuple from to
  return [dec]
  where
    tvns    = map tyVarBndrName tvbs
    cont    = foldl' appT (conT nm) (map varT tvns)

    isTuple from to =
      instanceD
        (cxt (map (\t -> [t| Elt $(varT t) |]) tvns))
        [t| IsTuple $cont |]
        [ tySynInstD (tySynEqn Nothing [t| TupleR $cont |] tupleR)
        , funD (mkName "fromTup") from
        , funD (mkName "toTup") to
        ]

    tupleR =
      let tt = foldl' (\ts t -> [t| ( $ts, $(recT t) ) |]) (tupleT 0) (concat fts)
       in if st
            then [t| (TAG, $tt) |]
            else tt

    fieldTys (NormalC _ fs) = map snd fs
    fieldTys (RecC _ fs)    = map (\(_,_,t) -> t) fs
    fieldTys (InfixC a _ b) = [snd a, snd b]
    fieldTys _              = fail "mkIsTuple: only constructors for \"vanilla\" syntax are supported"

    recT t
      | Just n <- tyConName t, elem n pts = [t| Rec $(return t) |]
      | otherwise                         = return t

    n   = length cs
    st  = n > 1
    fts = map fieldTys cs
    cts = constructorTags n

    go prev (this:next) (con:cons) (tag:tags) = do
      r  <- mkCon st pts tvns tag prev next con
      rs <- go (this:prev) next cons tags
      return (r : rs)
    go _ [] [] [] = return []
    go _ _  _  _  = fail "mkIsTuple: unexpected error"

mkCon :: Bool -> [Name] -> [Name] -> Word8 -> [[Type]] -> [[Type]] -> Con -> Q (ClauseQ, ClauseQ)
mkCon st pts tvs tag prev next = \case
  NormalC nm fs -> mkNormalC st pts tvs tag nm prev (map snd fs) next
  RecC nm fs    -> fail "mkIsTuple: TODO: record syntax"
  InfixC a nm b -> fail "mkIsTuple: TODO: infix constructors"
  _             -> fail "mkIsTuple: only constructors for \"vanilla\" syntax are supported"

mkNormalC :: Bool -> [Name] -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (ClauseQ, ClauseQ)
mkNormalC st pts tvs tag nm fs0 fs fs1 = do
  xs <- replicateM (length fs) (newName "_x")
  let
      fromTup = clause [lhs] body []
        where
          lhs  = conP nm (map varP xs)
          body = normalB
               . sumE
               . foldl' (\es e -> [| ($es, $e) |]) (tupE [])
               $ map (\t -> [| toElt (undef (eltR @ $(recT t))) |]) (concat (reverse fs0))
              ++ zipWith recE fs xs
              ++ map (\t -> [| toElt (undef (eltR @ $(recT t))) |]) (concat fs1)

          sumE e
            | st        = [| ( $(litE (IntegerL (toInteger tag))), $e) |]
            | otherwise = e

      toTup = clause [lhs] body []
        where
          sumP p
            | st        = tupP [ litP (IntegerL (toInteger tag)), p ]
            | otherwise = p

          lhs  = sumP
               . foldl' (\ps p -> [p| ($ps, $p) |]) (tupP [])
               $ map (const wildP) (concat (reverse fs0))
              ++ zipWith recP fs xs
              ++ map (const wildP) (concat fs1)

          body = normalB
               $ foldl' appE (conE nm)
               $ map varE xs
  --
  return (fromTup, toTup)

  where
    recT t
      | Just n <- tyConName t, elem n pts = [t| Rec $(return t) |]
      | otherwise                         = return t

    recE t x
      | Just n <- tyConName t, elem n pts = [| Rec $(varE x) |]
      | otherwise                         = varE x

    recP t x
      | Just n <- tyConName t, elem n pts = [p| Rec $(varP x) |]
      | otherwise                         = varP x

    vs = reverse
       $ [ Nothing | _ <- concat fs0 ] ++ [ Just f | f <- fs ] ++ [ Nothing | _ <- concat fs1 ]

