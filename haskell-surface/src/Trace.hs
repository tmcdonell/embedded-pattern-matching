{-# LANGUAGE GADTs #-}

module Trace where

import Rec
import Type
import {-# SOURCE #-} Elt

type TAG = Word8

-- XXX: This structure both witnesses the layout of our representation
-- types (as TypeR does) and represents a complete path of pattern matching
-- through this type. It indicates which fields of the structure represent
-- the union tags (TraceRtag) or store undefined values (TraceRundef).
--
-- The function 'traceR' produces all valid paths through the type. For
-- example the type '(Bool,Bool)' produces the following:
--
--   ghci> putStrLn . unlines . map show $ traceR @(Bool,Bool)
--   (((),(0#,())),(0#,()))     -- (False, False)
--   (((),(0#,())),(1#,()))     -- (False, True)
--   (((),(1#,())),(0#,()))     -- (True, False)
--   (((),(1#,())),(1#,()))     -- (True, True)
--
-- In the paper we did not discuss the TraceRundef constructor. It is not
-- necessary for the technique, it is just to keep track of values which
-- refer to undefined values because the constructor they are associated
-- with is not valid. For example:
--
--   ghci> putStrLn . unlines . map show $ traceR @(Maybe Int)
--   (0#,((),undef))            -- Nothing
--   (1#,((),.))                -- Just
--
data TraceR a where
  TraceRunit   :: TraceR ()
  TraceRprim   :: PrimType a -> TraceR a
  TraceRrec    :: TypeR (EltR a) -> TraceR (Rec a)
  TraceRundef  :: TypeR a -> TraceR a
  TraceRtag    :: TAG -> TraceR a -> TraceR (Word8, a)
  TraceRpair   :: TraceR a -> TraceR b -> TraceR (a, b)

instance Show (TraceR a) where
  show TraceRunit         = "()"
  show (TraceRrec _)      = "*"
  show (TraceRprim _)     = "."
  show (TraceRundef _)    = "undef"
  show (TraceRtag t a)    = "(" ++ show t ++ "#," ++ show a ++ ")"
  show (TraceRpair ta tb) = "(" ++ show ta ++ "," ++ show tb ++ ")"

