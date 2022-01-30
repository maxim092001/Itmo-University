module HW0.T5 ( Nat (..)
              , nz
              , ns
              , nplus
              , nmult
              , nFromNatural
              , nToNum
              ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz f x = x

ns :: Nat a -> Nat a
ns f x y = x (f x y)

nplus :: Nat a -> Nat a -> Nat a
nplus x y f z = y f (x f z)

nmult :: Nat a -> Nat a -> Nat a
nmult x y f = x (y f)

nFromNatural :: Natural -> Nat a
nFromNatural x = case x of
  0 -> nz
  x -> ns $ nFromNatural (x - 1)

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0
