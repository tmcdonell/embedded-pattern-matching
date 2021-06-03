{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module TH.Elt (
  mkElt,
  mkElt',
) where

import Elt
import Rec
import Trace
import Type
import TH.Common

import Data.List
import Control.Monad
import Language.Haskell.TH                        hiding ( Exp, Match, match, recP )


mkElt :: Name -> DecsQ
mkElt nm = mkElt' nm []

mkElt' :: Name -> [Name] -> DecsQ
mkElt' nm pts = do
  info <- reify nm
  case info of
    TyConI dec -> mkDec pts dec
    _          -> fail "mkElt: expected the name of a newtype or datatype"

mkDec :: [Name] -> Dec -> DecsQ
mkDec pts dec =
  case dec of
    DataD    _ nm tv _ cs _ -> mkDataD nm (nm:pts) tv cs
    NewtypeD _ nm tv _ c  _ -> mkNewtypeD nm pts tv c
    _                       -> fail "mkElt: expected the name of a newtype or datatype"

mkNewtypeD :: Name -> [Name] -> [TyVarBndr] -> Con -> DecsQ
mkNewtypeD nm pts tvbs c = mkDataD nm (nm:pts) tvbs [c]

mkDataD :: Name -> [Name] -> [TyVarBndr] -> [Con] -> DecsQ
mkDataD nm pts tvbs cs = do
  (from, to, trace) <- unzip3 <$> go [] fts cs cts
  dec               <- mk from to trace
  return [dec]
  where
    tvns    = map tyVarBndrName tvbs
    cont    = foldl' appT (conT nm) (map varT tvns)

    mk from to trace =
      instanceD
        (cxt (map (\t -> [t| Elt $(varT t) |]) tvns))
        [t| Elt $cont |]
        [ tySynInstD (tySynEqn Nothing [t| EltR $cont |] mkEltRT)
        , funD (mkName "fromElt") from
        , funD (mkName "toElt") to
        , funD (mkName "eltR") [mkEltR]
        , funD (mkName "traceR") [clause [] (normalB [| concat $(listE trace)|]) []]
        ]

    mkEltRT = sumT $ foldl' (\ts t -> [t| ( $ts, $(recT t) ) |]) (tupleT 0) (concat fts)
      where
        recT t
          | Just n <- tyConName t, elem n pts = [t| Rec $(return t) |]
          | otherwise                         = [t| EltR $(return t) |]

        sumT t | st        = [t| (TAG, $t) |]
               | otherwise = t

    mkEltR = clause [] body []
      where
        sumE e
          | st        = [| TypeRpair (TypeRprim primType) $e |]
          | otherwise = e

        recE t
          | Just n <- tyConName t, elem n pts = [| TypeRrec (eltR @ $(return t)) |]
          | otherwise                         = [| eltR @ $(return t) |]

        body = normalB
             . sumE
             . foldl' (\es e -> [| TypeRpair $es $e |]) [| TypeRunit |]
             $ map recE (concat fts)

    fieldTys (NormalC _ fs) = map snd fs
    fieldTys (RecC _ fs)    = map (\(_,_,t) -> t) fs
    fieldTys (InfixC a _ b) = [snd a, snd b]
    fieldTys _              = fail "mkElt: only constructors for \"vanilla\" syntax are supported"

    nc  = length cs
    st  = nc > 1
    fts = map fieldTys cs
    cts = constructorTags nc

    go prev (this:next) (con:cons) (tag:tags) = do
      r  <- mkCon st pts tvns tag prev next con
      rs <- go (this:prev) next cons tags
      return (r : rs)
    go _ [] [] [] = return []
    go _ _  _  _  = fail "mkElt: unexpected error"

mkCon :: Bool -> [Name] -> [Name] -> Word8 -> [[Type]] -> [[Type]] -> Con -> Q (ClauseQ, ClauseQ, ExpQ)
mkCon st pts tvs tag prev next = \case
  NormalC nm fs -> mkNormalC st pts tvs tag nm prev (map snd fs) next
  RecC nm fs    -> fail "mkElt: TODO: record syntax"
  InfixC a nm b -> fail "mkElt: TODO: infix constructors"
  _             -> fail "mkElt: only constructors for \"vanilla\" syntax are supported"

mkNormalC :: Bool -> [Name] -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (ClauseQ, ClauseQ, ExpQ)
mkNormalC st pts tvs tag nm fs0 fs fs1 =
  return (mkFromElt, mkToElt, mkTrace)
  where
    mkFromElt = do
      xs <- replicateM (length fs) (newName "_x")
      clause [lhs xs] (body xs) []
      where
        sumE e
          | st        = [| ( $(litE tagL), $e) |]
          | otherwise = e

        recT t
          | Just n <- tyConName t, elem n pts = [t| Rec $(return t) |]
          | otherwise                         = return t

        recE t x
          | Just n <- tyConName t, elem n pts = [| Rec $(varE x) |]
          | otherwise                         = [| fromElt $(varE x) |]

        lhs  xs = conP nm (map varP xs)
        body xs = normalB
                . sumE
                . foldl' (\es e -> [| ($es, $e) |]) (tupE [])
                $ map (\t -> [| undef (eltR @ $(recT t)) |]) (concat (reverse fs0))
               ++ zipWith recE fs xs
               ++ map (\t -> [| undef (eltR @ $(recT t)) |]) (concat fs1)

    mkToElt = do
      xs <- replicateM (length fs) (newName "_x")
      clause [lhs xs] (body xs) []
      where
        sumP p
          | st        = tupP [ litP tagL, p ]
          | otherwise = p

        sumE t x
          | Just n <- tyConName t, elem n pts = varE x
          | otherwise                         = [| toElt $(varE x) |]

        recP t x
          | Just n <- tyConName t, elem n pts = [p| Rec $(varP x) |]
          | otherwise                         = varP x

        lhs xs = sumP
               . foldl' (\ps p -> [p| ($ps, $p) |]) (tupP [])
               $ map (const wildP) (concat (reverse fs0))
              ++ zipWith recP fs xs
              ++ map (const wildP) (concat fs1)

        body xs = normalB
                . foldl' appE (conE nm)
                $ zipWith sumE fs xs

    mkTrace = sumE
            . foldl' (\es e -> [| TraceRpair <$> $es <*> $e |]) [| pure TraceRunit |]
            $ map (\t -> [| [ TraceRundef (eltR @ $(recT t)) ] |]) (concat (reverse fs0))
           ++ map recE fs
           ++ map (\t -> [| [ TraceRundef (eltR @ $(recT t)) ] |]) (concat fs1)
      where
        sumE e | st        = [| TraceRtag $(litE tagL) <$> $e |]
               | otherwise = e

        recT t
          | Just n <- tyConName t, elem n pts = [t| Rec $(return t) |]
          | otherwise                         = return t

        recE t
          | Just n <- tyConName t, elem n pts = [| pure (TraceRrec (eltR @ $(return t))) |]
          | otherwise                         = [| traceR @ $(return t) |]

    tagL = IntegerL (toInteger tag)

