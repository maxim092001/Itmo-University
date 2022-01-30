{-# LANGUAGE InstanceSigs #-}

module HW1.T7 ( ListPlus (..)
              , DotString (..)
              , Inclusive (..)
              , Fun (..)
              , unwrapFun
              ) where

data ListPlus a = a :+ ListPlus a | Last a deriving Show
infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) :: ListPlus a -> ListPlus a -> ListPlus a
  (<>) (Last x) lst    = x :+ lst
  (<>) (h1 :+ t1) lst2 = h1 :+ (t1 <> lst2)

data Inclusive a b = This a | That b | Both a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) :: Inclusive a b -> Inclusive a b -> Inclusive a b
  (<>) (That a) (That b)     = That (a <> b)
  (<>) (This a) (This b)     = This (a <> b)
  (<>) (This a) (That b)     = Both a b
  (<>) (That a) (This b)     = Both b a
  (<>) (Both a b) (This x)   = Both (a <> x) b
  (<>) (Both a b) (That x)   = Both a (b <> x)
  (<>) (This x) (Both a b)   = Both (x <> a) b
  (<>) (That x) (Both a b)   = Both a (x <> b)
  (<>) (Both a b) (Both x y) = Both (a <> x) (b <> y)


newtype DotString = DS String deriving Show

instance Semigroup DotString where
  (<>) :: DotString -> DotString-> DotString
  (<>) (DS "") s       = s
  (<>) s (DS "")       = s
  (<>) (DS s1) (DS s2) = DS(s1 ++ "." ++ s2)

instance Monoid DotString where
  mempty :: DotString
  mempty = DS ""

newtype Fun a = F (a -> a)

unwrapFun :: Fun a -> (a -> a)
unwrapFun (F f) = f

instance Semigroup (Fun a) where
  (<>) :: Fun a -> Fun a -> Fun a
  (<>) (F f) (F g) = F (f . g)

instance Monoid (Fun a) where
  mempty :: Fun a
  mempty = F id

