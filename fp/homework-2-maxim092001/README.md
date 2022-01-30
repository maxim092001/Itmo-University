#### Вторая лабораторная работа

### Task 1

1. Create a module named HW2.T1 and define the following data types in it:

-	```hs 
	data Option a = None | Some a
	```

-	```hs
	data Pair a = P a a
	```
	
-	```hs
	data Quad a = Q a a a a
	```
	
-	```hs
	data Annotated e a = a :# e
	infix 0 :#
	```
	
-	```hs
	data Except e a = Error e | Success a
	```

-	```hs
	data Prioritised a = Low a | Medium a | High a
	```

-	```hs
	data Stream a = a :> Stream a
	infixr 5 :>
	```

-	```hs
	data List a = Nil | a :. List a
	infixr 5 :.
	```

-	```hs
	data Fun i a = F (i -> a)
	```

-	```hs
	data Tree a = Leaf | Branch (Tree a) a (Tree a)
	```

2. For each of those types, implement a function of the following form:
	
	```hs
	mapF :: (a -> b) -> (F a -> F b)
	```
	That is, implement the following functions:
	```hs
	mapOption      :: (a -> b) -> (Option a -> Option b)
	mapPair        :: (a -> b) -> (Pair a -> Pair b)
	mapQuad        :: (a -> b) -> (Quad a -> Quad b)
	mapAnnotated   :: (a -> b) -> (Annotated e a -> Annotated e b)
	mapExcept      :: (a -> b) -> (Except e a -> Except e b)
	mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
	mapStream      :: (a -> b) -> (Stream a -> Stream b)
	mapList        :: (a -> b) -> (List a -> List b)
	mapFun         :: (a -> b) -> (Fun i a -> Fun i b)
	mapTree        :: (a -> b) -> (Tree a -> Tree b)
	```
	These functions must modify only the elements and preserve the overall structure (e.g. do not reverse the list, do not rebalance the tree, do not swap the pair).

	This property is witnessed by the following laws:
	
	```
		 mapF id  ≡  id
	mapF f ∘ mapF g   ≡  mapF (f ∘ g)
	```

	You must implement these functions by hand, without using any predefined functions (not even from `Prelude`) or deriving.

### Task 2

Create a module named HW2.T2. For each type from the first task except Tree, implement functions of the following form:

```hs
distF :: (F a, F b) -> F (a, b)
wrapF :: a -> F a
```

That is, implement the following functions:
```hs
distOption      :: (Option a, Option b) -> Option (a, b)
distPair        :: (Pair a, Pair b) -> Pair (a, b)
distQuad        :: (Quad a, Quad b) -> Quad (a, b)
distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distExcept      :: (Except e a, Except e b) -> Except e (a, b)
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distStream      :: (Stream a, Stream b) -> Stream (a, b)
distList        :: (List a, List b) -> List (a, b)
distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)

wrapOption      :: a -> Option a
wrapPair        :: a -> Pair a
wrapQuad        :: a -> Quad a
wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapExcept      :: a -> Except e a
wrapPrioritised :: a -> Prioritised a
wrapStream      :: a -> Stream a
wrapList        :: a -> List a
wrapFun         :: a -> Fun i a
```
The following laws must hold:

  - Homomorphism:

    `distF (wrapF a, wrapF b)  ≅  wrapF (a, b)`

  - Associativity:

    `distF (p, distF (q, r))   ≅  distF (distF (p, q), r)`

  - Left and right identity:
	```
	distF (wrapF (), q)  ≅  q
	distF (p, wrapF ())  ≅  p
	```

In the laws stated above, we reason up to the following isomorphisms:

```
((a, b), c)  ≅  (a, (b, c))  -- for associativity
    ((), b)  ≅  b            -- for left identity
    (a, ())  ≅  a            -- for right identity
```
There is more than one way to implement some of these functions. In addition to the laws, take the following expectations into account:

  - `distPrioritised` must pick the higher priority out of the two.
  - `distList` must associate each element of the first list with each element of the second list (i.e. the resulting list is of length n × m).

You must implement these functions by hand, using only:

  - data types that you defined in HW2.T1
  - `(<>)` and `mempty` for `Annotated`

Task 3

Create a module named HW2.T3. For `Option`, `Except`, `Annotated`, `List`, and `Fun` define a function of the following form:

```hs
joinF :: F (F a) -> F a
```

That is, implement the following functions:

```hs
joinOption    :: Option (Option a) -> Option a
joinExcept    :: Except e (Except e a) -> Except e a
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinList      :: List (List a) -> List a
joinFun       :: Fun i (Fun i a) -> Fun i a
```
The following laws must hold:

- Associativity:

	`joinF (mapF joinF m)  ≡  joinF (joinF m)`

	In other words, given `F (F (F a))`, it does not matter whether we join the outer layers or the inner layers first.

- Left and right identity:
	```
	joinF      (wrapF m)  ≡  m
	joinF (mapF wrapF m)  ≡  m
	```
	In other words, layers created by wrapF are identity elements to joinF.

	Given `F a`, you can add layers outside/inside to get `F (F a)`, but `joinF` flattens it back into `F a` without any other changes to the structure.

Furthermore, `joinF` is strictly more powerful than `distF` and can be used to define it:

```
distF (p, q) = joinF (mapF (\a -> mapF (\b -> (a, b)) q) p)
```

At the same time, this is only one of the possible `distF` definitions (e.g. List admits at least two lawful distF). It is common in Haskell to expect `distF` and `joinF` to agree in behavior, so the above equation must hold. (Do not redefine `distF` using `joinF`, though: it would be correct but not the point of the exercise).

#### Task 4

1. Create a module named HW2.T4 and define the following data type in it:

	```hs
	data State s a = S { runS :: s -> Annotated s a }
	```
    
2. Implement the following functions:

	```hs
	mapState :: (a -> b) -> State s a -> State s b
	wrapState :: a -> State s a
	joinState :: State s (State s a) -> State s a
	modifyState :: (s -> s) -> State s ()
	```
	
	Using those functions, define `Functor`, `Applicative`, and `Monad` instances:

	```hs
	instance Functor (State s) where
		fmap = mapState

	instance Applicative (State s) where
		pure = wrapState
		p <*> q = Control.Monad.ap p q

	instance Monad (State s) where
		m >>= f = joinState (fmap f m)
	```

	These instances will enable the use of do-notation with `State`.

	The semantics of `State` are such that the following holds:
	
	```
	runS (do modifyState f; modifyState g; return a) x
		≡
	a :# g (f x)
	```
	
	In other words, we execute stateful actions left-to-right, passing the state from one to another.

3. Define the following data type, representing a small language:

	```hs
	data Prim a =
           Add a a      -- (+)
         | Sub a a      -- (-)
         | Mul a a      -- (*)
         | Div a a      -- (/)
         | Abs a        -- abs
         | Sgn a        -- signum

	data Expr = Val Double | Op (Prim Expr)
	```
	For notational convenience, define the following instances:

	```hs
	instance Num Expr where
	  x + y = Op (Add x y)
	  x * y = Op (Mul x y)
	  ...
	  fromInteger x = Val (fromInteger x)

   instance Fractional Expr where
   ...
	```
	So that `(3.14 + 1.618 :: Expr)` produces this syntax tree:

	```
	Op (Add (Val 3.14) (Val 1.618))
	```

4. Using do-notation for `State` and combinators we defined for it (`pure`, `modifyState`), define the evaluation function:

	```hs
	eval :: Expr -> State [Prim Double] Double
	```

	In addition to the final result of evaluating an expression, it accumulates a trace of all individual operations:

	```
	runS (eval (2 + 3 * 5 - 7)) []
		≡
	10 :# [Sub 17 7, Add 2 15, Mul 3 5]
	```
	
	The head of the list is the last operation, this way adding another operation to the trace is `O(1)`.

	You can use the trace to observe the evaluation order. Consider this expression:
	```
	(a * b) + (x * y)
	```
	In eval, we choose to evaluate `(a * b)` first and `(x * y)` second, even though the opposite is also possible and would not affect the final result of the computation.

#### Task 5

1. Create a module named HW2.T5 and define the following data type in it:

	```hs
    data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }
	```
    This type is a combination of `Except` and `State`, allowing a stateful computation to abort with an error.

2. Implement the following functions:
	```hs
    mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
    wrapExceptState :: a -> ExceptState e s a
    joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
    modifyExceptState :: (s -> s) -> ExceptState e s ()
    throwExceptState :: e -> ExceptState e s a
	```

	Using those functions, define `Functor`, `Applicative`, and `Monad` instances.

3. Using do-notation for `ExceptState` and combinators we defined for it (`pure`, `modifyExceptState`, `throwExceptState`), define the evaluation function:

	```hs
    data EvaluationError = DivideByZero
    eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
	```

    It works just as eval from the previous task but aborts the computation if division by zero occurs:
	```
		runES (eval (2 + 3 * 5 - 7)) []
			≡
		Success (10 :# [Sub 17 7, Add 2 15, Mul 3 5])
	```
	```
		runES (eval (1 / (10 - 5 * 2))) []
			≡
		Error DivideByZero
	```
### Task 6

1. Create a module named HW2.T6 and define the following data type in it:

	```hs
    data ParseError = ErrorAtPos Natural

    newtype Parser a = P (ExceptState ParseError (Natural, String) a)
      deriving newtype (Functor, Applicative, Monad)
	```
    Here we use `ExceptState` for an entirely different purpose: to parse data from a string. Our state consists of a `Natural` representing how many characters we have already consumed (for error messages) and the `String` is the remainder of the input.

2. Implement the following function:
	```hs
    runP :: Parser a -> String -> Except ParseError a
	```

3. Let us define a parser that consumes a single character:
	```hs
    pChar :: Parser Char
    pChar = P $ ES \(pos, s) ->
      case s of
        []     -> Error (ErrorAtPos pos)
        (c:cs) -> Success (c :# (pos + 1, cs))
	```
    Study this definition:
      - What happens when the string is empty?
      - How does the parser state change when a character is consumed?

    Write a comment that explains `pChar`.

4. Implement a parser that always fails:
	```hs
    parseError :: Parser a
	```

    Define the following instance:
	```hs
    instance Alternative Parser where
      empty = parseError
      (<|>) = ...
	
    instance MonadPlus Parser   -- No methods.
	```

    So that `p <|> q` tries to parse the input string using `p`, but in case of failure tries `q`.

    Make sure that the laws hold:
	```
    empty <|> p  ≡  p
    p <|> empty  ≡  p
	```
5. Implement a parser that checks that there is no unconsumed input left (i.e. the string in the parser state is empty), and fails otherwise:
	```hs
    pEof :: Parser ()
	```
6. Study the combinators provided by `Control.Applicative` and `Control.Monad`. The following are of particular interest:
	- `msum`
	- `mfilter`
	- `optional`
	- `many`
	- `some`
	- `void`


    We can use them to construct more interesting parsers. For instance, here is a parser that accepts only non-empty sequences of uppercase letters:
	
	```hs
    pAbbr :: Parser String
    pAbbr = do
      abbr <- some (mfilter Data.Char.isUpper pChar)
      pEof
      pure abbr
	```

    It can be used as follows:
		
	```hs
    ghci> runP pAbbr "HTML"
    Success "HTML"

    ghci> runP pAbbr "JavaScript"
    Error (ErrorAtPos 1)
	```

7. Using parser combinators, define the following function:
	```hs
    parseExpr :: String -> Except ParseError Expr
	```
    It must handle floating-point literals of the form 4.09, the operators + - * / with the usual precedence (multiplication and division bind tighter than addition and subtraction), and parentheses.

    Example usage:
	```hs
    ghci> parseExpr "3.14 + 1.618 * 2"
    Success (Op (Add (Val 3.14) (Op (Mul (Val 1.618) (Val 2.0)))))

    ghci> parseExpr "2 * (1 + 3)"
    Success (Op (Mul (Val 2.0) (Op (Add (Val 1.0) (Val 3.0)))))

    ghci> parseExpr "24 + Hello"
    Error (ErrorAtPos 3)
	```
    The implementation must not use the `Read` class, as it implements similar functionality (the exercise is to write your own parsers). At the same time, you are encouraged to use existing `Applicative` and `Monad` combinators, since they are not specific to parsing.
 
