{-# LANGUAGE GADTs #-}

module Type (
  module Type,
  Word8,
) where

import Data.Word
import Data.Typeable

data TypeR a where
  TypeRunit :: TypeR ()
  TypeRprim :: PrimType a -> TypeR a
  TypeRpair :: TypeR a -> TypeR b -> TypeR (a,b)

data PrimType a where
  IntegralNumType :: IntegralType a -> PrimType a
  FloatingNumType :: FloatingType a -> PrimType a

data IntegralType a where
  TypeInt   :: IntegralType Int
  TypeWord8 :: IntegralType Word8

data FloatingType a where
  TypeFloat :: FloatingType Float

class IsPrim a where
  primType :: PrimType a

instance IsPrim Int   where primType = IntegralNumType TypeInt
instance IsPrim Word8 where primType = IntegralNumType TypeWord8
instance IsPrim Float where primType = FloatingNumType TypeFloat

instance Show (TypeR a) where
  show TypeRunit         = "()"
  show (TypeRprim t)     = show t
  show (TypeRpair ta tb) = "(" ++ show ta ++ "," ++ show tb ++ ")"

instance Show (PrimType a) where
  show (IntegralNumType t) = show t
  show (FloatingNumType t) = show t

instance Show (IntegralType a) where
  show TypeInt   = "Int"
  show TypeWord8 = "Word8"

instance Show (FloatingType a) where
  show TypeFloat = "Float"

data TypeableDict a where
  TypeableDict :: Typeable a => TypeableDict a

typeableDict :: TypeR a -> TypeableDict a
typeableDict TypeRunit = TypeableDict
typeableDict (TypeRpair ta tb)
  | TypeableDict <- typeableDict ta
  , TypeableDict <- typeableDict tb
  = TypeableDict
typeableDict (TypeRprim t) = prim t
  where
    prim :: PrimType t -> TypeableDict t
    prim (IntegralNumType x) = integral x
    prim (FloatingNumType x) = floating x

    integral :: IntegralType t -> TypeableDict t
    integral TypeInt   = TypeableDict
    integral TypeWord8 = TypeableDict

    floating :: FloatingType t -> TypeableDict t
    floating TypeFloat = TypeableDict

