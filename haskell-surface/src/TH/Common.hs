{-# LANGUAGE TemplateHaskell  #-}

module TH.Common where

import Data.Bits
import Data.List
import Data.Word
import Language.Haskell.TH


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
-- XXX: This is incorrect for n > 6, so we should fix it
--
constructorTags :: Int -> [Word8]
constructorTags n =
  let
      m = n `quot` 2
      l = take m     (iterate (True:) [False])
      r = take (n-m) (iterate (True:) [True])

      bitsToTag = foldl' f 0
        where
          f i False =         i `shiftL` 1
          f i True  = setBit (i `shiftL` 1) 0
  in
  map bitsToTag (l ++ r)


tupT :: [TypeQ] -> TypeQ
tupT tup =
  let n = length tup
   in foldl' (\ts t -> [t| $ts $t |]) (tupleT n) tup

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV  n)   = n
tyVarBndrName (KindedTV n _) = n

tyConName :: Type -> Maybe Name
tyConName (ConT n)        = Just n
tyConName (InfixT _ n _)  = Just n
tyConName (UInfixT _ n _) = Just n
tyConName (AppT t _)      = tyConName t
tyConName (ParensT t)     = tyConName t
tyConName _               = Nothing

