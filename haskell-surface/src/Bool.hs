{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices    #-}

module Bool (
  Bool(..),
  pattern False_,
  pattern True_,
) where

import Elt
import Tuple
import TH

instance Elt Bool
instance IsTuple Bool

mkPattern ''Bool

{--
pattern False_ :: Exp Bool
pattern False_ <- (matchFalse -> Just ())
  where False_ = buildFalse

pattern True_ :: Exp Bool
pattern True_ <- (matchTrue -> Just ())
  where True_ = buildTrue
{-# COMPLETE False_, True_ #-}

tag :: TAG -> Tuple TAG
tag x = Exp $ Const x

buildFalse :: Exp Bool
buildFalse = Tuple $ tag 0 `Pair` Unit

buildTrue :: Exp Bool
buildTrue = Tuple $ tag 1 `Pair` Unit

matchFalse :: Exp Bool -> Maybe ()
matchFalse (Match (TraceRtag 0 _) _) = Just ()
matchFalse Match{}                   = Nothing
matchFalse _                         = error "matchFalse: used outside 'match' context"

matchTrue :: Exp Bool -> Maybe ()
matchTrue (Match (TraceRtag 1 _) _) = Just ()
matchTrue Match{}                   = Nothing
matchTrue _                         = error "matchTrue: used outside 'match' context"
--}

