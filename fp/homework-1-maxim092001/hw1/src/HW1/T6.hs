module HW1.T6 ( mcat
              , epart
              ) where

import Data.Foldable (fold)

mcat :: Monoid a => [Maybe a] -> a
mcat  = foldr mcat' mempty where
  mcat' :: Monoid a => Maybe a -> a -> a
  mcat' Nothing acc = acc
  mcat' (Just x) acc = x <> acc

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldl epart' (mempty, mempty) where
  epart' (l, r) x = case x of
                      Left a  -> (l <> a, r)
                      Right a -> (l, r <> a)
