module HW2.T1 ( Option (..)
              , Pair (..)
              , Quad (..)
              , Annotated (..)
              , Except (..)
              , Prioritised (..)
              , Stream (..)
              , List (..)
              , Fun (..)
              , Tree (..)
              , mapOption
              , mapPair
              , mapQuad
              , mapAnnotated
              , mapExcept
              , mapPrioritised
              , mapStream
              , mapList
              , mapFun
              , mapTree
              ) where

data Option a = None | Some a deriving Show

data Pair a = P a a deriving Show

data Quad a = Q a a a a deriving Show

data Annotated e a = a :# e deriving Show
infix 0 :#

data Except e a = Error e | Success a deriving Show

data Prioritised a = Low a | Medium a | High a deriving Show

data Stream a = a :> Stream a deriving Show
infixr 5 :>

data List a = Nil | a :. List a deriving Show
infixr 5 :.

newtype Fun i a = F (i -> a)

data Tree a = Leaf | Branch (Tree a) a (Tree a) deriving Show

-- All map* functions modify only elements inside and do not change the overall structure.
--          mapF id  ≡  id
-- mapF f ∘ mapF g   ≡  mapF (f ∘ g)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f None     = None
mapOption f (Some x) = Some $ f x

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a1 a2) = P (f a1) (f a2)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a1 a2 a3 a4) = Q (f a1) (f a2) (f a3) (f a4)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)   = Error e
mapExcept f (Success a) = Success $ f a

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a)    = Low $ f a
mapPrioritised f (Medium a) = Medium $ f a
mapPrioritised f (High a)   = High $ f a

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (x :> s) = f x :> mapStream f s

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil      = Nil
mapList f (a :. t) = f a :. mapList f t

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F g) = F (f . g)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf           = Leaf
mapTree f (Branch l x r) = Branch (mapTree f l) (f x) (mapTree f r)
