{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -ddump-splices    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Either where

import Elt
import Tuple
import TH

import Type
import Trace
import Exp

instance (Elt a, Elt b) => Elt (Either a b)
instance (Elt a, Elt b) => IsTuple (Either a b)

mkPattern ''Either

