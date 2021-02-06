
module Rec where

data Rec a = Rec a
data Fix f = Fix (f (Fix f))

