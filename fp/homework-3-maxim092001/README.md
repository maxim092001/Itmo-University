### Третья лабораторная работа


In this homework we will gradually develop a small programming language called Hi.

#### Project Structure

Create a .cabal file with both a library and an executable:

```
library
  exposed-modules:    ...
  build-depends:      ...
  ...

executable hi
  main-is:            Main.hs
  hs-source-dirs:     ...
  build-depends:      ...
  ...
```

In the library component, create the following modules:
```
HW3.Base
HW3.Parser
HW3.Pretty
HW3.Evaluator
```
You are allowed to add more modules to the project, but those are the required ones.

  - In HW3.Base, define the following data types:
	```hs
    data HiFun     -- function names (e.g. div, sort, length, ...)
    data HiValue   -- values (numbers, booleans, strings, ...)
    data HiExpr    -- expressions (literals, function calls, ...)
    data HiError   -- evaluation errors (invalid arguments, ...)
	```
    In each task, we will add constructors to these data types that are needed to implement new language features.

  - In HW3.Parser, define the following function:
	```hs
    parse :: String -> Either (ParseErrorBundle String Void) HiExpr
	```

    The `ParseErrorBundle` type comes from the `megaparsec` package which we will use to implement our parser.

  - In HW3.Pretty, define the following function:

	```hs
    prettyValue :: HiValue -> Doc AnsiStyle
	```
    
	The `Doc` and `AnsiStyle` types come from the `prettyprinter` and `prettyprinter-ansi-terminal` packages respectively. This function renders a value to a document, which in turn can be either printed to the terminal (with color highlighting) or converted to a string.

  - In HW3.Evaluator, define the following function:

	```hs
    eval :: Monad m => HiExpr -> m (Either HiError HiValue)
	```
    
	One might wonder why we need the `Monad m` part. Indeed, for arithmetic operations, a simpler type would be sufficient:

	```hs
    eval :: HiExpr -> Either HiError HiValue
	```

    However, the monadic context will come into play later, when we start implementing `IO` actions (file system access, random number generation, and so on).

The executable component consists just of a single file, `Main.hs`.

#### The REPL

Using the `haskeline` package, implement a REPL in `Main.hs` that uses `parse`, `eval`, and `prettyValue` defined above. It’s going to be just 15–20 lines of code, but you will use it all the time to test your implementation.

Here’s an example session that will become possible as soon as we implement arithmetic operations:
```
hi> mul(2, 10)
20
```
```
hi> sub(1000, 7)
993
```
```
hi> div(3, 5)
0.6
```

#### Task 1: Numbers and arithmetic

1. Extend the data types in HW3.Base to include the following constructors:

	```hs
    data HiFun =
      ...
      | HiFunDiv
      | HiFunMul
      | HiFunAdd
      | HiFunSub

    data HiValue =
      ...
      | HiValueNumber Rational
      | HiValueFunction HiFun

    data HiExpr =
      ...
      | HiExprValue HiValue
      | HiExprApply HiExpr [HiExpr]

    data HiError =
      ...
      | HiErrorInvalidArgument
      | HiErrorInvalidFunction
      | HiErrorArityMismatch
      | HiErrorDivideByZero
	```
    Numbers are represented using the `Rational` type from `Data.Ratio`.

2. In the parser, add support for the following constructs:

    - Built-in names `div`, `mul`, `add`, and `sub`, that are parsed into the corresponding `HiFun` constructors (and then wrapped in `HiValueFunction`).

    - Numeric literals, such as 2, 3.14, -1.618, or 1.2e5, that are parsed into `HiValueNumber` (tip: use `Text.Megaparsec.Char.Lexer.scientific`).

    - Function application `f(a, b, c, ...)` that is parsed into `HiExprApply`.

For example, the expression `div(add(10, 15.1), 3)` is represented by the following syntax tree:

```
HiExprApply (HiExprValue (HiValueFunction HiFunDiv))
  [
    HiExprApply (HiExprValue (HiValueFunction HiFunAdd))
      [
        HiExprValue (HiValueNumber (10 % 1)),
        HiExprValue (HiValueNumber (151 % 10))
      ],
    HiExprValue (HiValueNumber (3 % 1))
  ]
```

3. In the evaluator, implement the arithmetic operations:

    - `add(500, 12)` evaluates to 512 (addition)
    - `sub(10, 100)` evaluates to -90 (subtraction)
    - `mul(23, 768)` evaluates to 17664 (multiplication)
    - `div(57, 190)` evaluates to 0.3 (division)

	Nested function applications are allowed:

	- `div(add(mul(2, 5), 1), sub(11,6))` evaluates to 2.2
	
	The following errors must be returned as HiError:

    - `HiErrorArityMismatch`: functions called with an incorrect amount of arguments, e.g. `sub(1)` or `sub(1, 2, 3)`.
    - `HiErrorDivideByZero`: the div function is called with 0 as its second argument, e.g. `div(1, 0)` or `div(1, sub(5, 5))`.
    - `HiErrorInvalidFunction`: numbers are used in function positions, e.g. `15(2)`.
    - `HiErrorInvalidArgument`: functions are used in numeric positions, e.g. `sub(10, add)`.

You are advised to use the `ExceptT` monad transformer to propagate `HiError` through the evaluator.

4. In the pretty-printer, define the following special cases for rendering numbers:

    - integers: 42, -8, 15
    - finite decimal fractions: 3.14, -8.15, 77.01
    - fractions: 1/3, -1/7, 3/11
    - mixed fractions: 5 + 1/3, -10 - 1/7, 24 + 3/11

	You will find these functions useful:

    - `quotRem` from `Prelude`
    - `fromRationalRepetendUnlimited` from the `scientific` package

The following session in the REPL should be possible if you have implemented all of the above correctly:

```
hi> 100
100

hi> -15
-15

hi> add(100, -15)
85

hi> add(3, div(14, 100))
3.14

hi> div(10, 3)
3 + 1/3

hi> sub(mul(201, 11), 0.33)
2210.67
```

#### Task 2: Booleans and comparison

1. Extend the data types in HW3.Base to include the following constructors:

	```hs
    data HiFun =
      ...
      | HiFunNot
      | HiFunAnd
      | HiFunOr
      | HiFunLessThan
      | HiFunGreaterThan
      | HiFunEquals
      | HiFunNotLessThan
      | HiFunNotGreaterThan
      | HiFunNotEquals
      | HiFunIf

    data HiValue =
      ...
      | HiValueBool Bool
	```
2. In the parser, add support for the following constructs:

    - Built-in names `not`, `and`, `or`, `less-than`, `greater-than`, `equals`, `not-less-than`, `not-greater-than`, `not-equals`, `if`, that are parsed into the corresponding `HiFun` constructors.

    - Built-in names true and false that are parsed into `HiValueBool`.

3. In the evaluator, implement the new operations.

Boolean algebra:

  - `not(true)` evaluates to `false` (negation)
  - `and(true, false)` evaluates to `false` (conjunction)
  - `or(true, false)` evaluates to `true` (disjunction)

Equality checking:

  - `equals(10, 10)` evaluates to `true`
  - `equals(false, false)` evaluates to `true`
  - `equals(3, 10)` evaluates to `false`
  - `equals(1, true)` evaluates to `false` (no implicit cast)
	

Comparisons:

  - `less-than(3, 10)` evaluates to `true`
  - `less-than(false, true)` evaluates to `true`
  - `less-than(false, 0)` evaluates to true (`Bool` is less than `Number`)

Complements:

  - for all A B, `greater-than(A, B) ≡ less-than(B, A)` holds
  - for all A B, `not-equals(A, B) ≡ not(equals(A, B))` holds
  - for all A, B, `not-less-than(A, B) ≡ not(less-than(A, B))` holds
  - for all A, B, `not-greater-than(A, B) ≡ not(greater-than(A, B))` holds

Branching:

  - for all A B, `if(true, A, B) ≡ A` holds
  - for all A B, `if(false, A, B) ≡ B` holds


The following session in the REPL should be possible:

```
hi> false
false

hi> equals(add(2, 2), 4)
true

hi> less-than(mul(999, 99), 10000)
false

hi> if(greater-than(div(2, 5), div(3, 7)), 1, -1)
-1

hi> and(less-than(0, 1), less-than(1, 0))
false
```

Note also that functions are values:

```
hi> if(true, add, mul)
add

hi> if(true, add, mul)(10, 10)
20

hi> if(false, add, mul)(10, 10)
100
```

Functions can also be tested for equality:

```
hi> equals(add, add)
true

hi> equals(add, mul)
false
```

The check is trivial: a function is equal only to itself.

*Ordering of function symbols is implementation-defined, that is, it’s up to you whether `less-than(add, mul)` or `greater-than(add, mul)`.*

#### Task 3: Operators

In the parser, add support for infix operators. The precedence and associativity are the same as in Haskell.

For all A B:

	
- `A / B` parses to `div(A, B)`
- `A * B` parses to `mul(A, B)`
- `A + B` parses to `add(A, B)`
- `A - B` parses to `sub(A, B)`
- `A < B` parses to `less-than(A, B)`
- `A > B` parses to `greater-than(A, B)`
- `A >= B` parses to `not-less-than(A, B)`
- `A <= B` parses to `not-greater-than(A, B)`
- `A == B` parses to `equals(A, B)`
- `A /= B` parses to `not-equals(A, B)`
- `A && B` parses to `and(A, B)`
- `A || B` parses to `or(A, B)`


Tip: use `makeExprParser` from the `parser-combinators` package.

The following session in the REPL should be possible:

```hs
hi> 2 + 2
4

hi> 2 + 2 * 3
8

hi> (2 + 2) * 3
12

hi> 2 + 2 * 3 == (2 + 2) * 3
false

hi> 10 == 2*5 && 143 == 11*13
true
```

#### Task 4: Strings and slices

1. Extend the data types in HW3.Base to include the following constructors:
	```hs
	data HiFun =
		...
		| HiFunLength
		| HiFunToUpper
		| HiFunToLower
		| HiFunReverse
		| HiFunTrim

	data HiValue =
		...
		| HiValueNull
		| HiValueString Text
	```
	Strings are represented using the `Text` type from the `text` package.

2. In the parser, add support for the following constructs:

    - Bulit-in names `length`, `to-upper`, `to-lower`, `reverse`, `trim`, that are parsed into the corresponding `HiFun` constructors.

    - Built-in name null that is parsed into `HiValueNull`.

    - String literals, such as "hello", "42", or "header\nfooter", that are parsed into `HiValueString` (tip: use `Text.Megaparsec.Char.Lexer.charLiteral`).


3. In the evaluator, implement the new operations:

    - `length("Hello World")` evaluates to `11`
    - `to-upper("Hello World")` evaluates to `"HELLO WORLD"`
    - `to-lower("Hello World")` evaluates to `"hello world"`
    - `reverse("stressed")` evaluates to `"desserts"`
    - `trim(" Hello World ")` evaluates to `"Hello World"`

	Then overload existing operations to work on strings:

    - `"Hello" + "World"` evaluates to `"HelloWorld"`
    - `"Cat" * 5` evaluates to `"CatCatCatCatCat" (tip: use `stimes`)
    - `"/home/user" / "hi"` evaluates to `"/home/user/hi"`

	When a string is used as a function of one argument, perform a lookup:

    - `"Hello World"(0)` evaluates to `"H"`
    - `"Hello World"(7)` evaluates to `"o"`

	Out-of-bounds indexing returns `null`:

    - `"Hello World"(-1)` evaluates to `null`
    - `"Hello World"(99)` evaluates to `null`

	When a string is used as a function of two arguments, take a slice:

    - `"Hello World"(0, 5)` evaluates to `"Hello"`
    - `"Hello World"(2, 4)` evaluates to `"ll"`

4. (Advanced) When a slice index is negative, implement the Python semantics of indexing from the end of the string:

    - `"Hello World"(0, -4)` evaluates to `"Hello W"`
    - `"Hello World"(-4, -1)` evaluates to `"orl"`

	When a slice index is `null`, treat it as the start/end of the string:

    - `"Hello, World"(2, null)` evaluates to `"llo, World"`
    - `"Hello, World"(null, 5)` evaluates to `"Hello"`

The following session in the REPL should be possible:

```hs
hi> to-upper("what a nice language")(7, 11)
"NICE"

hi> "Hello" == "World"
false

hi> length("Hello" + "World")
10

hi> length("hehe" * 5) / 3
6 + 2/3
```
#### Task 5: Lists and folds

1. Extend the data types in HW3.Base to include the following constructors:

	```hs
	data HiFun =
		...
		| HiFunList
		| HiFunRange
		| HiFunFold

	data HiValue =
		...
		| HiValueList (Seq HiValue)
	```

	Lists are represented using the `Seq` type from the `containers` package.

2. In the parser, add support for the following constructs:

  - Built-in names `list`, `range`, `fold`, that are parsed into the corresponding `HiFun` constructors.

  - List literals, written as `[A, B, C, ...]`, that are parsed into function application `list(A, B, C, ...)`.

3. In the evaluator, implement the new operations:

  - `list(1, 2, 3)` constructs `HiValueList` containing `1, 2, 3`
  - `range(5, 10.3)` evaluates to `[5, 6, 7, 8, 9, 10]`
  - `fold(add, [11, 22, 33])` evaluates to `66`
  - `fold(mul, [11, 22, 33])` evaluates to `7986`
  - `fold(div, [11, 22, 33])` evaluates to `1/66` (left fold)

	Then overload existing operations to work on lists:

  - `length([1, true, "Hello"])` evaluates to `3`
  - `reverse([1, true, "Hello"])` evaluates to `["Hello", true, 1]`
  - `[1, 2] + [3, 4, 5]` evaluates to `[1, 2, 3, 4, 5]`
  - `[0, "x"] * 3` evaluates to `[0, "x", 0, "x", 0, "x"]` (tip: use `stimes`)

The following session in the REPL should be possible:

```hs
hi> list(1, 2, 3, 4, 5)
[ 1, 2, 3, 4, 5 ]

hi> fold(add, [2, 5] * 3)
21

hi> fold(mul, range(1, 10))
3628800

hi> [0, true, false, "hello", "world"](2, 4)
[ false, "hello" ]

hi> reverse(range(0.5, 70/8))
[ 8.5, 7.5, 6.5, 5.5, 4.5, 3.5, 2.5, 1.5, 0.5 ]
```

#### Task 6: Bytes and serialisation

Lists of bytes (numbers from 0 to 255) can be represented and processed more efficiently. Let us introduce a new value type for them.

1. Extend the data types in HW3.Base to include the following constructors:

	```hs
    data HiFun =
      ...
      | HiFunPackBytes
      | HiFunUnpackBytes
      | HiFunEncodeUtf8
      | HiFunDecodeUtf8
      | HiFunZip
      | HiFunUnzip
      | HiFunSerialise
      | HiFunDeserialise

    data HiValue =
      ...
      | HiValueBytes ByteString
	```
    Bytes are represented using the strict `ByteString` type from the `bytestring` package.

2. In the parser, add support for the following constructs:

	- Built-in names `pack-bytes`, `unpack-bytes`, `zip`, `unzip`, `encode-utf8`, `decode-utf8`, `serialise`, and `deserialise`, that are parsed into the corresponding `HiFun` constructors.

	-	Byte array literals, such as `[# 01 3f ec #]` that are parsed into `HiValueBytes`. Each element is a two-digit hexadecimal number.

3. In the evaluator, implement the new operations:

	-	`pack-bytes([ 3, 255, 158, 32 ])` evaluates to `[# 03 ff 9e 20 #]`
	- `unpack-bytes([# 10 20 30 #])` evaluates to `[16, 32, 48]`
	- `encode-utf8("Hello!")` evaluates to `[# 48 65 6c 6c 6f 21 #]`
	- `decode-utf8([# 48 65 6c 6c 6f #])` evaluates to `"Hello"`
	- `decode-utf8([# c3 28 #])` evaluates to `null` (invalid UTF-8 byte sequence)
	- `zip` compresses the bytes using the `zlib` package (specify `bestCompression`)
	- `serialise` turns any value into bytes using the serialise package
	- for all A, `unzip(zip(A)) ≡ A` holds
	- for all A, `deserialise(serialise(A)) ≡ A` holds

	Then overload existing operations to work on bytes:
  	-	`[# 00 ff #] + [# 01 e3 #]` evaluates to `[# 00 ff 01 e3 #]`
    - `[# 00 ff #] * 3` evaluates to `[# 00 ff 00 ff 00 ff #]` (tip: use `stimes`)

	When bytes are used as a function, perform indexing/slicing as with strings and lists:

	*	`[# 00 ff 01 e3 #](1)` evaluates to `255`
		
	* `[# 00 ff 01 e3 #](1,3)` evaluates to `[# ff 01 #]`


The following session in the REPL should be possible:

```hs
hi> pack-bytes(range(30, 40))
[# 1e 1f 20 21 22 23 24 25 26 27 28 #]

hi> zip(encode-utf8("Hello, World!" * 1000))
[# 78 da ed c7 31 0d 00 20 0c 00 30 2b f0 23 64 0e 30 00 df 92 25 f3 7f a0 82 af
   fd 1a 37 b3 d6 d8 d5 79 66 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88 88
   88 88 88 88 88 88 88 88 fc c9 03 ca 0f 3b 28 #]

hi> decode-utf8([# 68 69 #] * 5)
"hihihihihi"

hi> unzip([# 78 da 63 64 62 06 00 00 0d 00 07 #])
[# 01 02 03 #]
```

#### Task 7: File I/O

In this task we extend the language with I/O capabilities. We consider it to be the most important part of the homework and it is graded with extra points.

Let us start by creating a new type in HW3.Base that encodes the available actions:

```hs
data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
```
Now recall that the type of our eval function is as follows:

```hs
eval :: Monad m => HiExpr -> m (Either HiError HiValue)
```

We could just require m to be `IO` in order to execute the actions, but that would be bad design, as it would make it impossible to do evaluation in a pure, deterministic context (e.g. for tests). Instead, let us create a new class in `HW3.Base`:

```hs
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
```

One could imagine at least a few possible instances of this class:



  1. `IO`, where those actions could interact with the actual file system

  2. `Identity`, where the actions do nothing and return `null`

  3. `State FS`, where `FS` is a pure and deterministic in-memory simulation of the file system

  4. `ReaderT Permissions IO`, which can be more secure than `IO` by controlling whether the program has read-only or read-write access


While it would be useful to implement all of those, we shall limit ourselves to the last one to avoid making the task unnecessarily tedious.

In a new module `HW3.Action`, declare the following:

```hs
data HiPermission =
    AllowRead
  | AllowWrite

data PermissionException =
  PermissionRequired HiPermission

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }
```

Finally, let us change the type of `eval` as follows:

```hs
- eval ::   Monad m => HiExpr -> m (Either HiError HiValue)
+ eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
```

With those preliminaries out of the way, we can start integrating the actions into the rest of the language.

1. Extend the data types in HW3.Base to include the following constructors:


	```hs
	data HiFun =
		...
		| HiFunRead
		| HiFunWrite
		| HiFunMkDir
		| HiFunChDir

	data HiValue =
		...
		| HiValueAction HiAction

	data HiExpr =
		...
		| HiExprRun HiExpr
	```


2. In the parser, add support for the following constructs:
 
	 - Built-in names `read`, `write`, `mkdir`, `cd`, that are parsed into the corresponding `HiFun` constructors.

	- Built-in name `cwd` that is parsed into `HiValueAction HiActionCwd`.

	- `E!` notation that is parsed into `HiExprRun`, e.g. `read("hello.txt")!`, `mkdir("projects")!`, or `cwd!`.

3. In the evaluator, implement the new functions to return the corresponding actions:
	
	- `read("hi.txt")` evaluates to `read("hi.txt")`. While visually the same, internally the first one is `HiExprApply` and the second one is `HiValueAction`.
	- `write("hi.txt", "Hi!")` evaluates to `write("hi.txt", [# 48 69 21 #])`
	- `mkdir("dir")` evaluates to `mkdir("dir")`
	- `cd("dir")` evaluates to `cd("dir")`

	Then implement the `HiExprRun` construct, which should execute the action using `runAction` that we defined earlier.


4. Define the HiMonad HIO instance, such that:

	- `cwd!` returns the current working directory
	- `cd("mydir")!` changes the current working directory to `mydir`
	- `read("myfile")!` returns the contents of myfile (use `HiValueString` if the contents are valid UTF-8 and `HiValueBytes` otherwise)
	- `read("mydir")!` returns the directory listing of `mydir`
	- `write("myfile", "Hello")!` writes `"Hello"` to `myfile`
	- `mkdir("mydir")!` creates a new directory `mydir`

	Use the `directory` package to implement all of the above.

5. Implement permission control in `HiMonad HIO`, so that actions throw `PermissionException` (using `throwIO`) unless they are allowed.

  - `AllowRead` enables `cwd`, `cd`, `read`
  - `AllowWrite` enables `write`, `mkdir`

The following session in the REPL should be possible:

```hs
hi> mkdir("tmp")!
null

hi> read("tmp")!
[]

hi> mkdir("tmp/a")!
null

hi> mkdir("tmp/b")!
null

hi> read("tmp")!
[ "a", "b" ]

hi> write("tmp/hi.txt", "Hello")!
null

hi> cd("tmp")!
null

hi> read("hi.txt")!
"Hello"
```
Note that actions are just values and only `!` forces their execution:

```hs
hi> read
read

hi> read("hi.txt")
read("hi.txt")

hi> read("hi.txt")!
"Hello"
```

#### Task 8: Date and time

1. Extend the data types in HW3.Base to include the following constructors:

	```hs
    data HiFun =
      ...
      | HiFunParseTime

    data HiValue =
      ...
      | HiValueTime UTCTime

    data HiAction =
      ...
      | HiActionNow
	```
    Time is represented using the `UTCTime` type from the `time` package.

    Extend the data types in `HW3.Action` to include the following constructors:

	```hs
    data HiPermission =
      ...
      | AllowTime
	```

2. In the parser, add support for the following constructs:

	- Built-in name parse-time that is parsed into the corresponding HiFun constructor.

	- Built-in name now that is parsed into the corresponding HiAction constructor.

3. In the evaluator, implement `parse-time` using `readMaybe` to parse a `HiValueString` into a `HiValueTime`, or `HiValueNull` in case of failure.

4. In the `HiMonad HIO` instance, implement the `HiActionNow` to return the current system time. It requires the `AllowTime` permission.

5. In the evaluator, overload existing operations to work on time:

	- `parse-time("2021-12-15 00:00:00 UTC") + 1000` evaluates to `parse-time("2021-12-15 00:16:40 UTC")` (use `addUTCTime`)
 	- `parse-time("2021-12-15 00:37:51.000890793 UTC") - parse-time("2021-12-15 00:37:47.649047038 UTC")` evaluates to `3.351843755` (use `diffUTCTime`)

The following session in the REPL should be possible:

```hs
hi> now!
parse-time("2021-12-15 00:42:33.02949461 UTC")

hi> parse-time("2021-01-01 00:00:00 UTC") + 365 * 24 * 60 * 60
parse-time("2022-01-01 00:00:00 UTC")
```


#### Task 9: Random numbers

1. Extend the data types in HW3.Base to include the following constructors:

	```hs
    data HiFun =
      ...
      | HiFunRand

    data HiAction =
      ...
      | HiActionRand Int Int
	```

2. In the parser, add support for the built-in name `rand` that is parsed into the corresponding `HiFun` constructor.

3. In the evaluator, implement the new function:



	- `rand(0, 10)` evaluates to `rand(0, 10)`. While visually the same, internally the first one is `HiExprApply` and the second one is `HiValueAction`.

4. Extend the `HiMonad HIO` instance, so that:
	
	- `rand(0, 5)!` evaluates to `0`, `1`, `2`, `3`, `4`, or `5`
	- the distribution of random numbers is `uniform`

    Tip: use the `random` package.

The following session in the REPL should be possible:


```hs
hi> rand
rand

hi> rand(0, 10)
rand( 0, 10 )

hi> rand(0, 10)!
8

hi> rand(0, 10)!
3
```


#### Task 10: Short-circuit evaluation

1. Extend the data types in HW3.Base to include the following constructors:

	```hs
    data HiFun =
      ...
      | HiFunEcho

    data HiAction =
      ...
      | HiActionEcho Text
	```
2. In the parser, add support for the built-in name `echo` that is parsed into the corresponding `HiFun` constructor.

3. In the evaluator, implement the new function:
      - `echo("Hello")` evaluates to `echo("Hello")`. While visually the same, internally the first one is `HiExprApply` and the second one is `HiValueAction`.

4. Extend the `HiMonad HIO` instance, so that `echo("Hello")!` prints `Hello` followed by a newline to stdout. It requires the `AllowWrite` permission.

5. In the evaluator, ensure that `if(true, A, B)` does not evaluate `B`, and `if(false, A, B)` does not evaluate `A`.

    Then generalise `A && B` as follows:
				
	- if `A` is `false` or `null`, return `A` without evaluating `B`
	- otherwise, evaluate and return `B`

    Generalise `A || B` as follows:
  
	- if `A` is false or `null`, evaluate and return `B`
	- otherwise, return `A` without evaluating `B`


The following session in the REPL should be possible:

```hs
hi> echo
echo

hi> echo("Hello")
echo("Hello")

hi> echo("Hello")!
Hello
null

hi> "Hello"(0) || "Z"
"H"

hi> "Hello"(99) || "Z"
"Z"

hi> if(2 == 2, echo("OK")!, echo("WTF")!)
OK
null

hi> true || echo("Don't do this")!
true

hi> false && echo("Don't do this")!
false

hi> [# 00 ff #] && echo("Just do it")!
Just do it
null

```

Task 11: Dictionaries

1. Extend the data types in `HW3.Base` to include the following constructors:
	```hs
    data HiFun =
      ...
      | HiFunCount
      | HiFunKeys
      | HiFunValues
      | HiFunInvert

    data HiValue =
      ...
      | HiValueDict (Map HiValue HiValue)

    data HiExpr =
      ...
      | HiExprDict [(HiExpr, HiExpr)]
	```
    Dictionaries are represented using the `Map` type from the containers package.

2. In the parser, add support for the following constructs:

      - Built-in names `count`, `keys`, `values`, `invert`, that are parsed into the corresponding `HiFun` constructors.

      - Dictionary literals, written as `{ I: A, J: B, K: C }`, for example:
          - `{ "width": 120, "height": 80 }`
          - `{ 1: true, 3: true, 4: false }`

      - Dot access, written as `E.fld`, that is parsed into function application `E("fld")``. For example, the following holds:

		```hs
		{ "width": 120, "height": 80 }.width
			≡
		{ "width": 120, "height": 80 }("width")
		```
3. In the evaluator, implement the new operations:
      - `{ "width": 120, "height": 80 }("width")` evaluates to `120`
      - `keys({ "width": 120, "height": 80 })` evaluates to `["height", "width"]` (sorted)
      - `values({ "width": 120, "height": 80 })` evaluates to `[80, 120] (sorted by key)`
      - `count("XXXOX")` evaluates to `{ "O": 1, "X": 4 }`
      - `count([# 58 58 58 4f 58 #])` evaluates to `{ 79: 1, 88: 4 }`
      - `count([true, true, false, true])` evaluates to `{ false: 1, true: 3 }`
      - `invert({ "x": 1, "y" : 2, "z": 1 })` evaluates to `{ 1: [ "z", "x" ], 2: ["y"] }`

The following session in the REPL should be possible:

```hs
hi> count("Hello World").o
2

hi> invert(count("big blue bag"))
{ 1: [ "u", "l", "i", "e", "a" ], 2: [ "g", " " ], 3: ["b"] }

hi> fold(add, values(count("Hello, World!")))
13

```
