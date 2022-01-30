module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , ncmp
  , nFromNatural
  , nToNum
  , nEven
  , nOdd
  , nmod
  , ndiv
  ) where

import Numeric.Natural

data N = Z | S N deriving (Show)

nplus :: N -> N -> N
nplus Z a         = a
nplus a Z         = a
nplus (S a) (S b) = S $ S $ nplus a b

nmult :: N -> N -> N
nmult Z _         = Z
nmult _ Z         = Z
nmult (S a) (S Z) = S a
nmult (S a) (S b) = nplus (S a) $ nmult (S a) b

nsub :: N -> N -> Maybe N
nsub Z Z         = Just Z
nsub Z _         = Nothing
nsub (S a) Z     = Just (S a)
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp _ Z         = GT
ncmp Z _         = LT
ncmp (S a) (S b) = ncmp a b

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural $ pred n

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = 1 + nToNum a

-- advanced
nEven, nOdd :: N -> Bool
nEven Z         = True
nEven (S Z)     = False
nEven (S (S a)) = nEven a

nOdd = not . nEven

ndiv :: N -> N -> N
ndiv a b = case nsub a b of
  Nothing -> Z
  Just Z  ->  S Z
  Just x  ->  S $ ndiv x b

nmod :: N -> N -> N
nmod a b = case nsub a b of
  Nothing -> a
  Just Z  -> Z
  Just x  -> nmod x b

