### Первая лабораторная работа 

#### Task 1

1. Create a module named HW1.T1 and define the following data type in it:

	```hs
	data Day = Monday | Tuesday | ... | Sunday
	```

	(Obviously, fill in the ... with the rest of the week days).

	Do not derive Enum for Day, as the derived toEnum is partial:

	```hs
	ghci> toEnum 42 :: Day
	*** Exception: toEnum{Day}: tag (42) is outside of enumeration's range (0,6)
	```

2. Implement the following functions:

	```hs
	-- | Returns the day that follows the day of the week given as input.
	nextDay :: Day -> Day

	-- | Returns the day of the week after a given number of days has passed.
	afterDays :: Natural -> Day -> Day

	-- | Checks if the day is on the weekend.
	isWeekend :: Day -> Bool

	-- | Computes the number of days until Friday.
	daysToParty :: Day -> Natural
	```

	In daysToParty, if it is already Friday, the party can start immediately, we don’t have to wait for the next week (i.e. return 0 rather than 7).

	*Good job if you spotted that this task is a perfect fit for modular arithmetic, but that is not the point of the exercise. The functions must be implemented by operating on Day values directly, without conversion to a numeric representation.*

#### Task 2

1. Create a module named HW1.T2 and define the following data type in it:

	```hs
	data N = Z | S N
	```

2. Implement the following functions:
	```hs
	nplus :: N -> N -> N        -- addition
	nmult :: N -> N -> N        -- multiplication
	nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)
	ncmp :: N -> N -> Ordering  -- comparison      (Do not derive Ord)
	```
	The operations must be implemented without using built-in numbers (Int, Integer, Natural, and such).

3. Implement the following functions:
	```hs
	nFromNatural :: Natural -> N
	nToNum :: Num a => N -> a
	```
4. (Advanced) Implement the following functions:
	```hs
	nEven, nOdd :: N -> Bool    -- parity checking
	ndiv :: N -> N -> N         -- integer division
	nmod :: N -> N -> N         -- modulo operation
	```
	The operations must be implemented without using built-in numbers.

	*In ndiv and nmod, the behavior in case of division by zero is not specified (you can throw an exception, go into an inifnite loop, or delete all files on the computer).*

#### Task 3

1. Create a module named HW1.T3 and define the following data type in it:

	```hs
	data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
	```
	The Meta field must store additional information about the subtree that can be accessed in constant time, for example its size. You can use Int, (Int, Int), or a custom data structure:

	```hs
	type Meta = Int         -- OK
	data Meta = M Int Int   -- OK
	```
	Functions operating on this tree must maintain the following invariants:
	1. **Sorted:** The elements in the left subtree are less than the head element of a branch, and the elements in the right subtree are greater.
	2. **Unique:** There are no duplicate elements in the tree (follows from Sorted).
	3. **CachedSize:** The size of the tree is cached in the Meta field for constant-time access.
	4. (Advanced) **Balanced:** The tree is balanced according to one of the following strategies:
		- **SizeBalanced:** For any given Branch _ l _ r, the ratio between the size of l and the size of r never exceeds 3.
		- **HeightBalanced:** For any given Branch _ l _ r, the difference between the height of l and the height of r never exceeds 1.

	These invariants enable efficient processing of the tree.

2. Implement the following functions:
	```hs
	-- | Size of the tree, O(1).
	tsize :: Tree a -> Int

	-- | Depth of the tree.
	tdepth :: Tree a -> Int

	-- | Check if the element is in the tree, O(log n)
	tmember :: Ord a => a -> Tree a -> Bool

	-- | Insert an element into the tree, O(log n)
	tinsert :: Ord a => a -> Tree a -> Tree a

	-- | Build a tree from a list, O(n log n)
	tFromList :: Ord a => [a] -> Tree a
	```

	Tip 1: in order to maintain the **CachedSize** invariant, define a helper function:

	```hs
	mkBranch :: Tree a -> a -> Tree a -> Tree a
	```
	Tip 2: the **Balanced** invariant is the hardest to maintain, so implement it last. Search for “tree rotation”.

#### Task 4

1. Create a module named HW1.T4.

2. Using the ```Tree``` data type from HW1.T3, define the following function:

	```hs
	tfoldr :: (a -> b -> b) -> b -> Tree a -> b
	```

	It must collect the elements in order:
	```hs
	treeToList :: Tree a -> [a]    -- output list is sorted
	treeToList = tfoldr (:) []
	```
	This follows from the **Sorted** invariant.

	You are encouraged to define ```tfoldr``` in an efficient manner, doing only a single pass over the tree and without constructing intermediate lists.

#### Task 5

1. Create a module named HW1.T5.

2. Implement the following function:

	```hs
	splitOn :: Eq a => a -> [a] -> NonEmpty [a]
	```

	Conceptually, it splits a list into sublists by a separator:

	```hs
	ghci> splitOn '/' "path/to/file"
	["path", "to", "file"]

	ghci> splitOn '/' "path/with/trailing/slash/"
	["path", "with", "trailing", "slash", ""]
	```

	Due to the use of ```NonEmpty``` to enforce that there is at least one sublist in the output, the actual GHCi result will look slightly differently:


	```hs
	ghci> splitOn '/' "path/to/file"
	"path" :| ["to","file"]
	```

	Do not let that confuse you. The first element is not in any way special.

3. Implement the following function:

	```hs
	joinWith :: a -> NonEmpty [a] -> [a]
	```

	It must be the inverse of splitOn, so that:

	```hs
	(joinWith sep . splitOn sep)  ≡  id
	```

	Example usage:
	```hs
	ghci> "import " ++ joinWith '.' ("Data" :| "List" : "NonEmpty" : [])
	"import Data.List.NonEmpty"
	```
#### Task 6

1. Create a module named HW1.T6.

2. Using ```Foldable``` methods *only*, implement the following function:

	```hs
	mcat :: Monoid a => [Maybe a] -> a
	```

	Example usage:
	```hs
	ghci> mcat [Just "mo", Nothing, Nothing, Just "no", Just "id"]
	"monoid"

	ghci> Data.Monoid.getSum $ mcat [Nothing, Just 2, Nothing, Just 40]
	42
	```
3. Using ```foldMap``` to consume the list, implement the following function:
	```hs
	epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
	```
	Example usage:
	```hs
	ghci> epart [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
	(Sum {getSum = 8},[1,2,3,4,5])
	```
#### Task 7

1. Create a module named HW1.T7.

2. Define the following data type and a lawful `Semigroup` instance for it:
	```hs
	data ListPlus a = a :+ ListPlus a | Last a
	infixr 5 :+
	```
3. Define the following data type and a lawful `Semigroup` instance for it:

	```hs
	data Inclusive a b = This a | That b | Both a b
	```
	The instance must not discard any values:

		```hs
		This i  <>  This j  =  This (i <> j)   -- OK
		This i  <>  This _  =  This i          -- This is not the Semigroup you're looking for.
		```
4. Define the following data type:

	```hs
	newtype DotString = DS String
	```

	Implement a `Semigroup` instance for it, such that the strings are concatenated with a dot:
	```hs
	ghci> DS "person" <> DS "address" <> DS "city"
	DS "person.address.city"
	```
	Implement a `Monoid` instance for it using DS "" as the identity element. Make sure that the laws hold:
	```hs
	mempty <> a  ≡  a
	a <> mempty  ≡  a
	```
5. Define the following data type:
	```hs
	newtype Fun a = F (a -> a)
	```
	Implement lawful Semigroup and Monoid instances for it.

