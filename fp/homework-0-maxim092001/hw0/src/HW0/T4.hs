module HW0.T4 ( repeat'
              , map'
              , fib
              , fac
              ) where

import Data.Function
import Numeric.Natural

repeat' :: a -> [a]
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]
map' f = fix (\r lst -> case lst of
                            (h : t) -> f h : r t
                            [] -> [])

fib :: Natural -> Natural
fib = fix (\r x y cnt -> if cnt == 0 then x else r y (x + y) (cnt - 1)) 0 1

fac :: Natural -> Natural
fac = fix (\r c -> if c == 0 then 1 else c * r (c - 1))
