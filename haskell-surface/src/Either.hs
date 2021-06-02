{-# OPTIONS_GHC -fno-warn-orphans #-}

module Either where

import Elt
import Tuple

instance (Elt a, Elt b) => Elt (Either a b)
instance (Elt a, Elt b) => IsTuple (Either a b)

