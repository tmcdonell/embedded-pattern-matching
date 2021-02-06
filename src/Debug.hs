
module Debug (
  module Debug,
  module Text.Printf,
) where

import Text.Printf
import qualified Debug.Trace as T

dEBUG :: Bool
dEBUG = False

trace :: String -> a -> a
trace msg x =
  if dEBUG
     then T.trace msg x
     else x

