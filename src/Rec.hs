
module Rec where

newtype Rec a = Rec a
newtype Fix f = Fix (f (Fix f))

