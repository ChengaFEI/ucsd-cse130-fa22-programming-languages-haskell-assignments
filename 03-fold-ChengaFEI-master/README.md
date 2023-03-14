# Assignment 3: All about Fold (160 points)

## Overview

The overall objective of this assignment is to expose you
to fold, *fold*, and more **fold**. And just when you think
you've had enough, **FOLD**.

The assignment is in the files:

1. [src/Hw3.hs](/src/Hw3.hs) has skeleton functions with
   missing bodies that you will fill in,
2. [tests/Test.hs](/tests/Test.hs) has some sample tests,
   and testing code that you will use to check your
   assignments before submitting.

You should only need to modify the parts of the files which say:

```haskell
error "TBD: ..."
```

with suitable Haskell implementations. Do not change the skeleton code,
like `... = foldLeft f base xs`, since the purpose is to get you used to
writing folds.

Exception: you may rename variables that we provide, which includes changing
their patterns. For example, if we give you `(_, res) = ...`, feel free to
change it to `(_, result) = ...` or `result = ...`. Feel free to add guards,
too.

You may also define helper variables in the where clause.

You are allowed to use any library function on integers,
but only the following library functions on lists: 

     length
     append (++)
     map
     foldl'
     foldr
     unzip
     zip
     reverse

If you know the functions `.` (compose) and `$` (apply), feel free to use them,
though they are not by any means necessary to complete this assignment.

**Note:** Start early, to avoid any unexpected shocks late in the day.

## Assignment Testing and Evaluation

All the points will be awarded automatically, by
**evaluating your functions against a given test suite**.

[Tests.hs](/tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
When you run

```shell
$ make test
```

Your last lines should have

```
All N tests passed (...)
OVERALL SCORE = ... / ...
```

**or**

```
K out of N tests failed
OVERALL SCORE = ... / ...
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error ...` with your partial
solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

## Submission Instructions

Submit your code via the HW-3 assignment on Gradescope.
You must submit a single zip file containing a single directory with your repository inside.
A simple way to create this zip file is:

- Run `git push` to push your local changes to your private fork of the assignment repository
- Navigate to your private fork on github and download source code as a zip

Please *do not* include the `.stack-work` or `__MACOSX` folders into the submission.

**Note:** Upon submission, Gradescope will only test your code on the *small public test suite*,
so it will show maximum 24/160 points.
After the deadline, we will regrade your submission on the full private test suite
and you will get your full points.

## Recommended Workflow

Learning folds comes with lots of type errors. You can make certain type errors
less nasty by annotating variables with their types. For example, in the
expression:

```haskell
f (g x) y
```

you can annotate any of the variables to prevent the compiler from assigning
them strange types.

```haskell
f (g x) (y :: Int)
f ((g :: [Int] -> Int) x) y
```

Additionally, to test the functions that you'll be writing, we recommend you
run `make ghci`. This brings up a prompt that automatically imports your functions.
Use this prompt to check any of the examples below.

## Problem 1: Warm-Up

### (a) 15 points

Fill in the skeleton given for `sqsum`,
which uses `foldl'` to get a function

```haskell
sqSum :: [Int] -> Int
```

such that `sqSum [x1,...,xn]` returns the integer `x1^2 + ... + xn^2`

Your task is to fill in the appropriate values for

1. the step function `f` and
2. the base case `base`.

Once you have implemented the function, you should get
the following behavior:

```haskell
ghci> sqSum []
0

ghci> sqSum [1, 2, 3, 4]
30

ghci> sqSum [(-1), (-2), (-3), (-4)]
30
```

### (b) 30 points

Fill in the skeleton given for `pipe` which uses `foldl'`
to get a function

```haskell
pipe :: [(a -> a)] -> (a -> a)
```

such that `pipe [f1,...,fn] x` (where `f1,...,fn` are functions!)
should return `f1(f2(...(fn x)))`.

Again, your task is to fill in the appropriate values for

1. the step function `f` and
2. the base case `base`.

Once you have implemented the function, you should get
the following behavior:

```haskell
ghci> pipe [] 3
3

ghci> pipe [(\x -> x+x), (\x -> x + 3)] 3
12

ghci> pipe [(\x -> x * 4), (\x -> x + x)] 3
24
```

**Hint**: if `pipe` throws the error `Couldn't match type a with a -> a`,
make sure your `f` is returning a function!

### (c) 20 points

Fill in the skeleton given for `sepConcat`,
which uses `foldl'` to get a function

```haskell
sepConcat :: String -> [String] -> String
```

Intuitively, the call `sepConcat sep [s1,...,sn]` where

* `sep` is a string to be used as a separator, and
* `[s1,...,sn]` is a list of strings

should behave as follows:


* `sepConcat sep []` should return the empty string `""`,
* `sepConcat sep [s]` should return just the string `s`,
* otherwise (if there is more than one string) the output
  should be the string `s1 ++ sep ++ s2 ++ ... ++ sep ++ sn`.

You should only modify the parts of the skeleton consisting
of `error "TBD" "`. You will need to define the function `f`,
and give values for `base` and `l`.

Once done, you should get the following behavior:

```haskell
ghci> sepConcat ", " ["foo", "bar", "baz"]
"foo, bar, baz"

ghci> sepConcat "---" []
""

ghci> sepConcat "" ["a", "b", "c", "d", "e"]
"abcde"

ghci> sepConcat "X" ["hello"]
"hello"
```

### (d) 10 points

Implement the function

```haskell
stringOfList :: (a -> String) -> [a] -> String
```

such that `stringOfList f [x1,...,xn]` should return the string
`"[" ++ (f x1) ++ ", " ++ ... ++ (f xn) ++ "]"`

This function can be implemented on one line,
**without using any recursion** by calling
`map` and `sepConcat` with appropriate inputs.

You should get the following behavior:

```haskell
ghci> stringOfList show [1, 2, 3, 4, 5, 6]
"[1, 2, 3, 4, 5, 6]"

ghci> stringOfList (\x -> x) ["foo"]
"[foo]"

ghci> stringOfList (stringOfList show) [[1, 2, 3], [4, 5], [6], []]
"[[1, 2, 3], [4, 5], [6], []]"
```

## Problem 2: Big Numbers

The Haskell type `Int` only contains values up to a certain size (for reasons
that will become clear as we implement our own compiler). For example,

```haskell
ghci> let x = 99999999999999999999999999999999999999999999999 :: Int

<interactive>:3:9: Warning:
    Literal 99999999999999999999999999999999999999999999999 is out of the Int range -9223372036854775808..9223372036854775807
```

You will now implement functions to manipulate arbitrarily large
(nonnegative, base-10) numbers represented as `[Int]`, i.e. lists of `Int`.

### (a) 10 + 5 + 10 points

Write a function

```haskell
clone :: a -> Int -> [a]
```

such that `clone x n` returns a list of `n` copies of the value `x`.
You may use recursion in the implementation of `clone` (though it is
not necessary).
If the integer `n` is `0` or negative, then `clone` should return
the empty list. You should get the following behavior:

```haskell
ghci> clone 3 5
[3, 3, 3, 3, 3]

ghci> clone "foo" 2
["foo", "foo"]
```

Use `clone` to write a function

```haskell
padZero :: [Int] -> [Int] -> ([Int], [Int])
```

which takes two lists: `[x1,...,xn]` `[y1,...,ym]` and
adds zeros in front of the _shorter_ list to make the
list lengths equal.

Your implementation should **not** be recursive.

You should get the following behavior:

```haskell
ghci> padZero [9, 9] [1, 0, 0, 2]
([0, 0, 9, 9], [1, 0, 0, 2])

ghci> padZero [1, 0, 0, 2] [9, 9]
([1, 0, 0, 2], [0, 0, 9, 9])
```

Next, write a function

```haskell
removeZero :: [Int] -> [Int]
```

that takes a list and removes a prefix of leading zeros, yielding
the following behavior:

```haskell
ghci> removeZero [0, 0, 0, 1, 0, 0, 2]
[1, 0, 0, 2]

ghci> removeZero [9, 9]
[9, 9]

ghci> removeZero [0, 0, 0, 0]
[]
```

**Note**: you may implement `removeZero` with recursion
(although it is certainly possible with a fold!)

### (b) 25 points

Let us use the list `[d1, d2, ..., dn]`, where each `di`
is between `0` and `9`, to represent the (positive)
**big-integer** `d1d2...dn`.

```haskell
type BigInt = [Int]
```

For example, `[9, 9, 9, 9, 9, 9, 9, 9, 9, 8]` represents
the big-integer `9999999998`. Fill out the implementation for

```haskell
bigAdd :: BigInt -> BigInt -> BigInt
```

so that it takes two integer lists, where each integer is
between `0` and `9` and returns the list corresponding to
the addition of the two big-integers. Again, you have to
fill in the implementation to supply the appropriate values
to `f`, `base`, `args`. You should get the following behavior:

```haskell
ghci> bigAdd [9, 9] [1, 0, 0, 2]
[1, 1, 0, 1]

ghci> bigAdd [9, 9, 9, 9] [9, 9, 9]
[1, 0, 9, 9, 8]
```
You may find the integer functions `div` and `mod` to be helpful here.

**Note about `BigInt`s**: we expect the result of `bigAdd` to not have any
leading zeroes, like `[0, 1, 0, 9, 9, 8]`. A caveat to this: zero should be
represented as `[]`.

### (c) 15 + 20 points

Next you will write functions to multiply two big integers.
First write a function

```haskell
mulByDigit :: Int -> BigInt -> BigInt
```

which takes an integer digit and a big integer, and returns the
big integer list which is the result of multiplying the big
integer with the digit. You should get the following behavior:

```haskell
ghci> mulByDigit 9 [9,9,9,9]
[8,9,9,9,1]
```

Now, using `mulByDigit`, fill in the implementation of

```haskell
bigMul :: BigInt -> BigInt -> BigInt
```

Again, you have to fill in implementations for `f` , `base` , `args` only.
Once you are done, you should get the following behavior at the prompt:

```haskell
ghci> bigMul [9,9,9,9] [9,9,9,9]
[9,9,9,8,0,0,0,1]

ghci> bigMul [9,9,9,9,9] [9,9,9,9,9]
[9,9,9,9,8,0,0,0,0,1]

ghci> bigMul [4,3,7,2] [1,6,3,2,9]
[7,1,3,9,0,3,8,8]

ghci> bigMul [9,9,9,9] [0]
[]
```
