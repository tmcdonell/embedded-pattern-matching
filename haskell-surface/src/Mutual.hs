{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Mutual where

import TH
import Elt
import Tuple

data A = AZ | AA A | AB B
data B = BZ | BB B | BA A

mkAll' ''A [''B]
mkAll' ''B [''A]

