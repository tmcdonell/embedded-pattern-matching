{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices    #-}

module Either where

import Elt
import Tuple
import TH

instance (Elt a, Elt b) => Elt (Either a b)
instance (Elt a, Elt b) => IsTuple (Either a b)

mkPattern ''Either

