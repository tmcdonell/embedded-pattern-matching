{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}

module Pretty where

import Elt
import Exp
import Rec
import Trace
import Tuple
import Type

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Lazy                           as T


instance Show (Exp a) where
  show = T.unpack . renderLazy . layoutSmart defaultLayoutOptions . ppExp

type Adoc = Doc AnsiStyle

ppExp :: forall a. Exp a -> Adoc
ppExp = \case
  Constant v  -> go (eltR @a) v
    where
      go :: TypeR t -> t -> Adoc
      go TypeRunit         ()      = "()"
      go (TypeRrec _)      (Rec _) = "Rec"
      go (TypeRpair ta tb) (a, b)  = tupled [go ta a, go tb b]
      go (TypeRprim t)     a       = prim t a

      prim :: PrimType t -> t -> Adoc
      prim (IntegralNumType x) = integral x
      prim (FloatingNumType x) = floating x

      integral :: IntegralType t -> t -> Adoc
      integral TypeInt   = pretty
      integral TypeWord8 = pretty

      floating :: FloatingType t -> t -> Adoc
      floating TypeFloat = pretty

  -- Unit        -> error "naked Unit"
  -- Pair{}      -> error "naked Pair"
  Tuple t     -> align . wrap $ collect t
    where
      wrap [x] = x
      wrap xs  = tupled xs
      --
      -- collect :: Exp t -> [Adoc]
      -- collect = \case
      --   Unit      -> []
      --   Pair x y  -> collect x ++ collect y
      --   other     -> [ppExp other]
      collect :: Tuple t -> [Adoc]
      collect = \case
        Unit     -> []
        Pair x y -> collect x ++ collect y
        Exp e    -> [ppExp e]

  Roll e      -> "&" <> ppExp e
  Unroll e    -> "*" <> ppExp e

  -- Unit        -> lparen <> rparen
  -- Pair l r    -> align . wrap . collect $ Pair l r

  Prj tix e   -> ppExp e <+> ppTupleIdx tix
  -- PrjL x      -> "prjL" <+> parens (ppExp x)
  -- PrjR x      -> "prjR" <+> parens (ppExp x)

  Var ix      -> ppIdx ix
  Let v a b   -> align . wrap . collect $ Let v a b
    where
      wrap ([bnd], body) = sep  [ nest shiftwidth (sep  [let_, bnd]), in_, body ]
      wrap (bnds,  body) = vsep [ nest shiftwidth (vsep (let_:bnds)), in_, body ]

      collect :: Exp t -> ([Adoc], Adoc)
      collect = \case
        Let v m n ->
          let m'              = ppExp m
              v'              = ppIdx v
              bnd | isLet m   = nest shiftwidth (vsep [v' <+> equals, m'])
                  | otherwise = v' <+> align (equals <+> m')
              (bnds, body)    = collect n
          in
          (bnd:bnds, body)

        other -> ([], ppExp other)

      isLet :: Exp t -> Bool
      isLet Let{} = True
      isLet _     = False

  App f x     -> ppExp f <+> ppExp x
  Lam ix x    -> "\\" <> ppIdx ix <+> "->" <+> ppExp x

  Undef{}     -> "undef"

  Case x xs ->
    nest shiftwidth
      . concatWith (\x y -> x <> hardline <> y)
      $ case_ <+> "tag#" <+> ppExp x <+> of_
      : map (\(t,e) -> ppTrace t <+> "->" <+> align (ppExp e)) xs

  -- Match{} -> error "Match: should not appear in the final AST"
  Match t e   -> annotate (color Red) "match" <+> ppTrace t <+> parens (ppExp e)

  Add x y     -> ppExp x <+> "+" <+> ppExp y


ppIdx :: Idx t -> Adoc
ppIdx (Idx nm) = pretty nm

ppTupleIdx :: TupleIdx t e -> Adoc
ppTupleIdx tix  = go tix
  where
    go :: TupleIdx t e -> Adoc
    go PrjZ     = "#"
    go (PrjL t) = go t <> "l"
    go (PrjR t) = go t <> "r"

    -- go :: TupleIdx t e -> Int
    -- go ZeroTupIdx     = 0
    -- go (SuccTupIdx t) = 1 + go t

ppTrace :: TraceR t -> Adoc
ppTrace tag = encloseSep "" "#" "." (go tag)
  where
    go :: TraceR s -> [Adoc]
    go TraceRunit         = []
    go (TraceRrec _)      = []
    go (TraceRprim _)     = []
    go (TraceRundef _)    = [pretty '.']
    go (TraceRtag t ta)   = pretty t : go ta
    go (TraceRpair ta tb) = go ta ++ go tb

shiftwidth = 2

let_  = annotate (colorDull Yellow) "let"
in_   = annotate (colorDull Yellow) "in"
case_ = annotate (colorDull Yellow) "case"
of_   = annotate (colorDull Yellow) "of"

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True  = group . parens . align
parensIf False = id

