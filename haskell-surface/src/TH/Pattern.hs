{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module TH.Pattern (
  mkPattern,
  mkPatternR,
) where

import Elt
import Exp
import Rec
import Trace
import Type
import TH.Common

import Data.List
import Control.Monad
import Language.Haskell.TH                        hiding ( Exp, Match, match )
import qualified Language.Haskell.TH              as TH


-- TODO (completed in the full implementation in Accelerate)
--
--   * Infix data constructors
--
--   * Named fields
--
--   * Check whether the type has the necessary class instances (Generic,
--     Show, etc.) and if not add standalone deriving instances
--
--   * Is it possible to add additional extensions pragmas to the file?
--
mkPattern :: Name -> DecsQ
mkPattern nm = mkPatternR nm []

mkPatternR :: Name -> [Name] -> DecsQ
mkPatternR nm rec = do
  info <- reify nm
  case info of
    TyConI dec -> mkDec rec dec
    _          -> fail "mkPattern: expected the name of a newtype or datatype"

mkDec :: [Name] -> Dec -> DecsQ
mkDec rec dec =
  case dec of
    DataD    _ nm tv _ cs _ -> mkDataD nm (nm:rec) tv cs
    NewtypeD _ nm tv _ c  _ -> mkNewtypeD nm rec tv c
    _                       -> fail "mkPattern: expected the name of a newtype or datatype"

mkNewtypeD :: Name -> [Name] -> [TyVarBndr] -> Con -> DecsQ
mkNewtypeD nm rec tvs c = mkDataD nm (nm:rec) tvs [c]

mkDataD :: Name -> [Name] -> [TyVarBndr] -> [Con] -> DecsQ
mkDataD tn rec tvs cs = do
  (pats, decs) <- unzip <$> go [] fts cs cts
  comp         <- pragCompleteD pats Nothing
  return $ comp : concat decs
  where
    fieldTys (NormalC _ fs) = map snd fs
    fieldTys (RecC _ fs)    = map (\(_,_,t) -> t) fs
    fieldTys (InfixC a _ b) = [snd a, snd b]
    fieldTys _              = fail "mkPattern: only constructors for \"vanilla\" syntax are supported"

    n   = length cs
    st  = n > 1
    fts = map fieldTys cs
    cts = constructorTags n

    go prev (this:next) (con:cons) (tag:tags) = do
      r  <- mkCon st tn rec tvs prev next tag con
      rs <- go (this:prev) next cons tags
      return (r : rs)
    go _ [] [] [] = return []
    go _ _  _  _  = fail "mkPattern: unexpected error"

mkCon :: Bool -> Name -> [Name] -> [TyVarBndr] -> [[Type]] -> [[Type]] -> Word8 -> Con -> Q (Name, [Dec])
mkCon st tn pts tvs prev next tag = \case
  NormalC nm fs -> mkNormalC st tn pts (map tyVarBndrName tvs) tag nm prev (map snd fs) next
  RecC nm fs    -> fail "mkPattern: TODO: record syntax"
  InfixC a nm b -> fail "mkPattern: TODO: infix constructors"
  _             -> fail "mkPattern: only constructors for \"vanilla\" syntax are supported"

mkNormalC :: Bool -> Name -> [Name] -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
mkNormalC st tn pts tvs tag cn ps fs ns = do
  (fun_mk,    dec_mk)    <- mkNormalC_build st tn pts tvs tag cn ps fs ns
  (fun_match, dec_match) <- mkNormalC_match st tn pts tvs tag cn ps fs ns
  (pat,       dec_pat)   <- mkNormalC_pattern tn tvs cn fs fun_mk fun_match
  return $ (pat, concat [dec_pat, dec_mk, dec_match])

mkNormalC_pattern :: Name -> [Name] -> Name -> [Type] -> Name -> Name -> Q (Name, [Dec])
mkNormalC_pattern tn tvs cn fs mk match = do
  xs <- replicateM (length fs) (newName "_x")
  r  <- sequence [ patSynSigD pat sig
                 , patSynD    pat
                     (prefixPatSyn xs)
                     (explBidir [clause [] (normalB (varE mk)) []])
                     (parensP $ viewP (varE match) [p| Just $(tupP (map varP xs)) |])
                 ]
  return (pat, r)
  where
    pat = mkName (nameBase cn ++ "_")
    sig = forallT
            (map plainTV tvs)
            (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
            (foldr (\t ts -> [t| $t -> $ts |])
                   [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                   (map (\t -> [t| Exp $(return t) |]) fs))

mkNormalC_build :: Bool -> Name -> [Name] -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
mkNormalC_build sum_type tn pts tvs tag cn fs0 fs fs1 = do
  fun <- newName ("_build" ++ nameBase cn)
  xs  <- replicateM (length fs) (newName "_x")
  let
    vs    = foldl' (\es e -> [| $es `Pair` $e |]) [| Unit |]
          $  map (\x -> [| Exp (Undef (eltR @ $(rec x))) |] ) (concat (reverse fs0))
          ++ zipWith (\t x -> [| Exp $(roll t x) |]) fs xs
          ++ map (\x -> [| Exp (Undef (eltR @ $(rec x))) |] ) (concat fs1)

    body  = clause (map varP xs) (normalB tagged) []
      where
        tagged
          | sum_type  = [| Tuple $ Exp (Const $(litE (IntegerL (toInteger tag)))) `Pair` $vs |]
          | otherwise = [| Tuple $vs |]

  r <- sequence [ sigD fun sig
                , funD fun [body]
                ]
  return (fun, r)
  where
    sig   = forallT
              (map plainTV tvs)
              (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
              (foldr (\t ts -> [t| $t -> $ts |])
                     [t| Exp $(foldl' appT (conT tn) (map varT tvs)) |]
                     (map (\t -> [t| Exp $(return t) |]) fs))

    rec t
      | Just n <- tyConName t, elem n pts = [t| Rec $(return t) |]
      | otherwise                         = return t

    roll t x
      | Just n <- tyConName t, elem n pts = [| Roll $(varE x) |]
      | otherwise                         = varE x

mkNormalC_match :: Bool -> Name -> [Name] -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
mkNormalC_match sum_type tn pts tvs tag cn fs0 fs fs1 = do
  fun     <- newName ("_match" ++ nameBase cn)
  e       <- newName "_e"
  x       <- newName "_x"
  (ps,es) <- extract x prj0 vs [] []
  let
    lhs   = varP e
    body  = normalB $ caseE (varE e)
      [ TH.match (conP 'Match [matchP ps, varP x]) (normalB [| Just $(tupE es) |]) []
      , TH.match (recP 'Match [])                  (normalB [| Nothing         |]) []
      , TH.match wildP                             (normalB [| error "Pattern synonym used outside 'match' context" |]) []
      ]

  r <- sequence [ sigD fun sig
                , funD fun [clause [lhs] body []]
                ]
  return (fun, r)
  where
    sig =
      forallT []
        (cxt (map (\t -> [t| Elt $(varT t) |]) tvs))
        [t| Exp $(foldl' appT (conT tn) (map varT tvs))
            -> Maybe $(tupT (map (\t -> [t| Exp $(return t) |]) fs)) |]

    matchP us
      | sum_type  = [p| TraceRtag $(litP (IntegerL (toInteger tag))) $pat |]
      | otherwise = pat
      where
        pat = [p| $(foldl (\ps p -> [p| TraceRpair $ps $p |]) [p| TraceRunit |] us) |]

    prj0 = [| PrjR PrjZ |]

    extract _ _   []     ps es = return (ps, es)
    extract x prj (u:us) ps es = do
      _u <- newName "_u"
      let next = [| PrjL $prj |]
          this | sum_type  = [| PrjR $prj |]
               | otherwise = prj
      case u of
        Nothing -> extract x next us (wildP  :ps) es
        Just t  -> extract x next us (varP _u:ps) ([| $(unroll t _u) (Prj $this $(varE x)) |] : es)

    unroll t u
      | Just n <- tyConName t, elem n pts = [| Unroll |]
      | otherwise                         = [| Match $(varE u) |]

    vs = reverse
       $ [ Nothing | _ <- concat fs0 ] ++ [ Just f | f <- fs ] ++ [ Nothing | _ <- concat fs1 ]

