
module TH (
  module TH
) where

import TH.Elt     as TH
import TH.IsTuple as TH
import TH.Pattern as TH

import Language.Haskell.TH

mkAll :: Name -> DecsQ
mkAll nm = mkAll' nm []

mkAll' :: Name -> [Name] -> DecsQ
mkAll' nm rec = do
  p <- mkPattern' nm rec
  e <- mkElt' nm rec
  t <- mkIsTuple' nm rec
  return (p ++ e ++ t)

