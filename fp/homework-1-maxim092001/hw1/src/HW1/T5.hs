module HW1.T5 ( splitOn
              , joinWith
              ) where

import Data.List.NonEmpty as NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delim = foldr splitOn' ([] :| []) where
  splitOn' x lst@(head :| tail)
    | x == delim = cons [] lst
    | otherwise = (x : head) :| tail


joinWith :: a -> NonEmpty [a] -> [a]
joinWith delim lst = concat (joinWith' [delim] (toList lst)) where
  joinWith' :: a -> [a] -> [a]
  joinWith' _ [] = []
  joinWith' delim (h:tail) = h : add' delim tail where
    add' :: a -> [a] -> [a]
    add' _ [] = []
    add' delim (head:tail) = delim : head : add' delim tail
