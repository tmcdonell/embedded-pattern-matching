{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module TH where

import Elt
import Exp
import Tag
import Type

import Data.Bits
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
mkPattern nm = do
  info <- reify nm
  case info of
    TyConI dec -> mkDec dec
    _          -> fail "mkPattern: expected the name of a newtype or datatype"

mkDec :: Dec -> DecsQ
mkDec dec =
  case dec of
    DataD    _ nm tv _ cs _ -> mkDataD nm tv cs
    NewtypeD _ nm tv _ c  _ -> mkNewtypeD nm tv c
    _                       -> fail "mkPatterns: expected the name of a newtype or datatype"

mkNewtypeD :: Name -> [TyVarBndr] -> Con -> DecsQ
mkNewtypeD tn tvs c = mkDataD tn tvs [c]

mkDataD :: Name -> [TyVarBndr] -> [Con] -> DecsQ
mkDataD tn tvs cs = do
  (pats, decs) <- unzip <$> go [] fts cs cts
  comp         <- pragCompleteD pats Nothing
  return $ comp : concat decs
  where
    fieldTys (NormalC _ fs) = map snd fs
    fieldTys (RecC _ fs)    = map (\(_,_,t) -> t) fs
    fieldTys (InfixC a _ b) = [snd a, snd b]
    fieldTys _              = error "mkPatterns: only constructors for \"vanilla\" syntax are supported"

    st  = length cs > 1
    fts = map fieldTys cs

    -- TODO: This backwards-engineers what tags GHC.Generics will give to
    -- the constructors. Instead, we should extract this directly by
    -- building a typeclass over the generics representation, e.g.:
    --
    -- class GConstructors f where
    --   constructors :: [(String, Word8)]
    --
    -- Which gives us a kind of lookup table. We can similarly extract
    -- the types of all the fields of the constructor. Can we use this
    -- information to build the pattern synonyms? Given a type as a Name
    -- can we run the appropriate instance of this type and get its result?
    -- Unknown...
    --
    -- https://stackoverflow.com/questions/27815489/is-it-possible-to-list-the-names-and-types-of-fields-in-a-record-data-type-that
    --
    cts =
      let n = length cs
          m = n `quot` 2
          l = take m     (iterate (True:) [False])
          r = take (n-m) (iterate (True:) [True])
      in
      map bitsToTag (l ++ r)

    bitsToTag = foldl' f 0
      where
        f n False =         n `shiftL` 1
        f n True  = setBit (n `shiftL` 1) 0

    go prev (this:next) (con:cons) (tag:tags) = do
      r  <- mkCon st tn tvs prev next tag con
      rs <- go (this:prev) next cons tags
      return (r : rs)
    go _ [] [] [] = return []
    go _ _  _  _  = fail "mkPatterns: unexpected error"

mkCon :: Bool -> Name -> [TyVarBndr] -> [[Type]] -> [[Type]] -> Word8 -> Con -> Q (Name, [Dec])
mkCon st tn tvs prev next tag = \case
  NormalC nm fs -> mkNormalC st tn (map tyVarBndrName tvs) tag nm prev (map snd fs) next
  RecC nm fs    -> fail "mkPatterns: TODO: record syntax"
  InfixC a nm b -> fail "mkPatterns: TODO: infix constructors"
  _             -> fail "mkPatterns: only constructors for \"vanilla\" syntax are supported"

mkNormalC :: Bool -> Name -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
mkNormalC st tn tvs tag cn ps fs ns = do
  (fun_mk,    dec_mk)    <- mkNormalC_mk st tn tvs tag cn ps fs ns
  (fun_match, dec_match) <- mkNormalC_match st tn tvs tag cn ps fs ns
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

mkNormalC_mk :: Bool -> Name -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
mkNormalC_mk sum_type tn tvs tag cn fs0 fs fs1 = do
  fun <- newName ("_mk" ++ nameBase cn)
  xs  <- replicateM (length fs) (newName "_x")
  let
    vs    = foldl' (\es e -> [| $es `Pair` $e |]) [| Unit |]
          $  map (\t -> [| Undef (eltR @ $(return t)) |] ) (concat (reverse fs0))
          ++ map varE xs
          ++ map (\t -> [| Undef (eltR @ $(return t)) |] ) (concat fs1)

    body  = clause (map (\x -> [p| (Exp $(varP x)) |]) xs) (normalB tagged) []
      where
        tagged
          | sum_type  = [| Exp $ (Constant (IntegralNumType TypeWord8) $(litE (IntegerL (toInteger tag)))) `Pair` $vs |]
          | otherwise = [| Exp $vs |]

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


mkNormalC_match :: Bool -> Name -> [Name] -> Word8 -> Name -> [[Type]] -> [Type] -> [[Type]] -> Q (Name, [Dec])
mkNormalC_match sum_type tn tvs tag cn fs0 fs fs1 = do
  fun     <- newName ("_match" ++ nameBase cn)
  e       <- newName "_e"
  x       <- newName "_x"
  (ps,es) <- extract vs (if sum_type then [| PrjR $(varE x) |] else varE x) [] []
  let
    lhs   = [p| (Exp $(varP e)) |]
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

    extract []     _ ps es = return (ps, es)
    extract (u:us) x ps es = do
      _u <- newName "_u"
      let x' = [| PrjL $x |]
      if not u
         then extract us x' (wildP:ps)  es
         else extract us x' (varP _u:ps) ([| Exp (Match $(varE _u) (PrjR $x)) |] : es)

    vs = reverse
       $ [ False | _ <- concat fs0 ] ++ [ True | _ <- fs ] ++ [ False | _ <- concat fs1 ]

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV  n)   = n
tyVarBndrName (KindedTV n _) = n

tupT :: [TypeQ] -> TypeQ
tupT tup =
  let n = length tup
   in foldl' (\ts t -> [t| $ts $t |]) (tupleT n) tup

