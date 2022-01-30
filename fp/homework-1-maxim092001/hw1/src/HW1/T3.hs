module HW1.T3 ( Tree(..)
          , tsize
          , tmember
          , tinsert
          , tFromList
          , tdepth
          , rotate
          , diffDepthPredicate
          , getLeft
          , getRight
          , smallRotateLeft
          , smallRotateRight
          , bigRotateLeft
          , bigRotateRight
          , rotateRightPredicate
          , rotateLeftPredicate
          ) where

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving Show

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l x r = Branch (tsize l + 1 + tsize r, max (tdepth l) (tdepth r) + 1) l x r

tsize :: Tree a -> Int
tsize Leaf                    = 0
tsize (Branch (sz, _) _ _ _ ) = sz

tdepth :: Tree a -> Int
tdepth Leaf                   = 0
tdepth (Branch (_, h) _ _ _ ) = h

tmember :: Ord a => a -> Tree a -> Bool
tmember a Leaf             = False
tmember a (Branch _ l x r) = (x == a) || tmember a l || tmember a r

isBalanced :: Ord a => Tree a -> Bool
isBalanced Leaf = True
isBalanced (Branch (sz, h) l x r)
  | isBalanced l && isBalanced r && abs (tdepth l - tdepth r) <= 1 = True
  | otherwise = False

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a t@(Branch _ l x r)
  | a < x = rotate $ mkBranch (tinsert a l) x r
  | a > x = rotate $ mkBranch l x (tinsert a r)
  | otherwise = t

-- tFromList [] = Leaf
-- tFromList (x : xs) = tinsert x (tFromList xs)

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

getLeft :: Tree a -> Tree a
getLeft Leaf             = Leaf
getLeft (Branch _ l _ _) = l

getRight :: Tree a -> Tree a
getRight Leaf             = Leaf
getRight (Branch _ _ _ r) = r

smallRotateLeft :: Ord a => Tree a -> Tree a
smallRotateLeft t@(Branch _ l x r@(Branch _ rl rx rr)) = mkBranch (mkBranch l x rl) rx rr
smallRotateLeft t                                      = t

smallRotateRight :: Ord a => Tree a -> Tree a
smallRotateRight t@(Branch _ l@(Branch _ ll lx lr) x r) = mkBranch ll lx (mkBranch lr x r)
smallRotateRight t                                      = t

bigRotateLeft :: Ord a => Tree a -> Tree a
bigRotateLeft t@(Branch _ l x r@(Branch _ rl@(Branch _ rll rlx rlr) rx rr)) = mkBranch (mkBranch l x rll) rlx (mkBranch rlr rx rr)
bigRotateLeft t = t

bigRotateRight :: Ord a => Tree a -> Tree a
bigRotateRight t@(Branch _ l@(Branch _ ll lx lr@(Branch _ lrl lrx lrr)) x r) = mkBranch (mkBranch ll lx lrl) lrx (mkBranch lrr x r)
bigRotateRight t = t

diffDepthPredicate :: Tree a -> Tree a -> Bool
diffDepthPredicate a b = tdepth a - tdepth b == 2

rotateRightPredicate :: Ord a => Tree a -> (Int -> Int -> Bool) -> Bool
rotateRightPredicate t@(Branch _ l _ r) cmp = diffDepthPredicate l r && tdepth (getRight $ getLeft t) `cmp` tdepth (getLeft $ getLeft t)

rotateLeftPredicate :: Ord a => Tree a -> (Int -> Int -> Bool) -> Bool
rotateLeftPredicate t@(Branch _ l _ r) cmp = diffDepthPredicate r l && tdepth (getLeft $ getRight t) `cmp` tdepth (getRight $ getRight t)

rotate :: Ord a => Tree a -> Tree a
rotate t@(Branch _ l _ r)
  | rotateLeftPredicate t (<=) = smallRotateLeft t
  | rotateLeftPredicate t (>) = bigRotateLeft t
  | rotateRightPredicate t (<=) = smallRotateRight t
  | rotateRightPredicate t (>) = bigRotateRight t
  | otherwise = t

