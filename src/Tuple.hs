{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Tuple where

-- data TupleIdx t e where
--   ZeroTupIdx ::                 TupleIdx (t, s) s
--   SuccTupIdx :: TupleIdx t e -> TupleIdx (t, s) e

data TupleIdx t e where
  PrjZ ::                 TupleIdx t      t
  PrjL :: TupleIdx l e -> TupleIdx (l, r) e
  PrjR :: TupleIdx r e -> TupleIdx (l, r) e

-- type family TupleR t :: *
-- type instance TupleR () = ()
-- type instance TupleR (a, b) = (((), a), b)
-- type instance TupleR (a, b, c) = ((((), a), b), c)

class IsTuple t where
  type TupleR t
  fromTup :: t -> TupleR t
  toTup   :: TupleR t -> t

instance IsTuple () where
  type TupleR () = ()
  fromTup        = id
  toTup          = id

instance IsTuple (a, b) where
  type TupleR (a, b) = (((), a), b)
  fromTup (a, b)     = (((), a), b)
  toTup (((), a), b) = (a, b)

instance IsTuple (a, b, c) where
  type TupleR (a, b, c)   = (TupleR (a, b), c)
  fromTup (a, b, c)       = ((((), a), b), c)
  toTup ((((), a), b), c) = (a, b, c)

