------
title: Advent of code 2024 problem 1.
date: 2024-11-17
tags: Tutorial, advent of code, 2024
------

This is still a draft!
====

This file can be read online [here](http://Luis-omega.github.io/Learning-Haskell/AdventOfCode/Blogs/AOC2024_2.html) or you can download the source file [here](https://github.com/Luis-omega/Learning-Haskell/AdventOfCode/Blogs/AOC2024_2.html). This blog entry was written in literate Haskell, it means that you can either read it or compile it to see how it works!

We are going to solve the first problem of [advent of code](https://adventofcode.com/2024/day/1) 2024.
First we are going follow a simple approach until we solve the full problem.
Then we are going to refactor our solution to experiment and to make it more `maintainable`, although that's not really needed we want to learn how to do that!

If you don't know what [Hoogle](https://hoogle.haskell.org/) is, you should get familiar with it as it is very useful to have it open while reading!

To begin, we create a new folder `AdventOfCode`, enter inside it and run `cabal init`

```bash
mkdir AdventOfCode
cd AdventOfCode
cabal init --libandexe --tests
```

It should generate a file structure like:

```bash
.
├── AdventOfCode.cabal
├── app
│   └── Main.hs
├── CHANGELOG.md
├── MyLibTest.hs
└── src
    └── MyLib.hs
```

Since we are going to use the `containers` package you may need to add it to the `library` section on `AdventOfCode.cabal`

```bash
library
    exposed-modules:  MyLib

    -- Modules included in this library but not exported.
    -- other-modules:

    -- LANGUAGE extensions used by modules in this package.
    -- other-extensions:
    build-depends:
      base ^>=4.14.3.0
      ,containers
    hs-source-dirs:   src
    default-language: Haskell2010
```

With this setup you should be able to run the application code doing `cabal run AdventOfCode` and just compiling it with `cabal build`.

I recommend you to rename the file `src/MyLib.hs` to something like `src/Problem1.hs` and to work on a single project per file.
If you choose to rename it, then be sure to reflect the change in `exposed-modules` under `library` on the `AdventOfCode.cabal` file.
Every time you create a new module you need to add it either here or in the `other-modules` section.

In every module we create we are going to export four functions `solve1`, `solve2`, `solve1_2` and `solve2_2`.

`solve1` and `solve1_2` are the solutions for the first part of the problem of the day.

`solve2` and `solve2_2` are the solutions for the second part of the problem of the day.

This means that at the top of `src/Problem1.hs` you should add them in the export list

```haskell
module Problem1(solve1,solve1_2,solve2,solve2_2) where
```

and of course, for now add dummy functions for them:

```haskell
solve1 :: IO ()
solve1 = undefined

solve1_2 :: IO ()
solve1_2 = undefined

solve2_1 :: IO ()
solve2_1 = undefined

solve2_2 :: IO ()
solve2_2 = undefined
```

Then under `app/Main.hs` you should modify it to import the four functions and to use one of them (in this case solve1)

```haskell
module Main where

import qualified Problem1 (solve1,solve1_2,solve2_1,solve2_2) qualified as P1

main :: IO ()
main = do
  P1.solve1
```

Now when you run `cabal run AdventOfCode` you should see something like:

```bash
cabal run AdventOfCode
AdventOfCode: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at src/Problem1.hs:4:12 in AdventOfCode-0.1.0.0-inplace:Problem1
```




Since I'm using literate Haskell for the entry, the following section is mandatory to be at the beginning, even if we don't discuss some of it right now!

\begin{code}
module Blogs.AOC2024_1 (solve1,solve2) where
import Data.List (sort)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
\end{code}

We are going to split the initial problem in three different parts:

- How we take a `String` representing an input file and transform it (parse) to something we can use to solve the problem.
- The logic of how to solve the problem.
- How we can actually read a file to a `String` and how we can show the final result on the console.

Parse Input
-----------

The input is expected to be on the form:

```txt
3   4
4   3
2   5
1   3
3   9
3   3
```

There is, every line represents a tuple of numbers `(a, b)` where `a` is at the beginning
without spaces, then we have some spaces followed by `b` and then we may have spaces or not until the end of the line.

This means that we can see the lines abstractly as:

```
Integer Spaces Integer MaybeSpaces LineBreak
```

In Haskell, we have the `words` function in the prelude (this means that you can use without importing anything unless you specifically enabled the option to hide the Haskell prelude).
From Hoogle

```
words breaks a string up into a list of words, which were delimited by white space
```

It means that it takes something like

```haskell
"a b cc dd   eef g    h"
```

and returns

```haskell
["a", "b", "cc", "dd", "eef", "g", "h"]
```

If we use `words` over a single line we got from a `String` with this form

```haskell
s = "Integer Spaces Integer MaybeSpaces LineBreak"
```

To a list like

```haskell
words s = [Integer, Integer]
```

Except that `words` do nothing to translate the string representing a `Integer` to a real number as understated by Haskell and keep them as `String`s.

As our next step we need to do two things:

- Change `[Integer, Integer]` (a list) to `(Integer, Integer)` (a tuple).
- Transform the `Integer` string to a number.

For the first one, we already know that input must contain only two numbers per line, not more, not less.
This means we can use a pattern match to extract the two values from the list

```haskell
let [valueLeft,valueRight] = words s
```

This is only a good idea in the context of advent of code where we know the input at all times but is a horrible idea to do this at production code! We are going to fix this later.

Then we need to translate both strings to integers. For this we are going to use the `read` function.
It takes a `String` and try to translate it to some Haskell value. We need to help it to understand what
we expect as result, so we put the type annotation `:: Int`. This is needed in the case you are
trying this in a REPL but in the full function you may skip this thanks to Haskell's type inference!


```haskell
let intLeft :: Int = read valueLeft
let intRight :: Int = read valueRight
```

With this we already read the two integers of the problem in a single line.
Since it's better to write a lot of small functions rather than a big one,
let's put all this in a single function

```haskell
parseLine :: String -> (Int,Int)
parseLine line =
  let
    [valueLeft,valueRight] = words line
    -- We can omit the type since we put the type of `parseLine` at the top!
    intLeft = read valueLeft
    intRight = read valueRight
  in
    (intLeft,intRight)
```

This function follows step by step what we discussed before, but is too big.
Instead we choose to write the following function:

\begin{code}
parseLine :: String -> (Int,Int)
parseLine line =
  let [valueLeft,valueRight] = words line
  in (read valueLeft, read valueRight)
\end{code}

This is more compact. Whether this style of the other is better depends on the context and the persons writing the code, but we are going to use this for now.

Now that we can parse a single line let's see how we can:

- Take a series of lines all together in a single `String`.
- Decompose the `String` to a list of strings (`[String]`) were every item of the list is a single line.
- Transform every line to a tuple `(Int, Int)`.
- Finally collect all the results to two lists of `Int`s (i.e. `([Int], [Int])`).

For the decomposition of the `String` we can simply use the `lines` function, it takes a `String` and return us a list of `Strings` with every `String` a single line, as we just wanted!

```haskell
let decomposed = lines s
```

Then we can use the `map` function to apply our previous function `parseLine` on every item of the list (i.e. on every line of the input) and collect the result on a new list:

```haskell
let listOfTuples = map parseLine decomposed
```

This means that `listOfTuples` is now a list of tuples of size two, with every item a `Int`, or in Haskell:

```haskell
listOfTuples :: [(Int, Int)]
```

A quick search in Hoogle also show us that we have available a function `unzip`. This function can take our list of tuples and split it in two list :

```haskell
let (leftList,rightList) = unzip listOfTuples
```

And now we have the two different lists of `Int` of the input!

```haskell
leftList :: [Int]
rightList :: [Int]
```

Putting all together:

```haskell
parseLists :: String -> ([Int],[Int])
parseLists input =
  let
    decomposed =  lines input
    listOfTuples = map parseLine decomposed
    (leftList,rightList) = unzip listOfTuples
  in
    (leftList,rightList)
```

However I'm a fan of the operators `<$>` and `$` that allow us to write it all in
a single line and still be readable.

For clarity we can translate the original `paseLists` in two steps.

First we delete all the intermediate variables and use parenthesis

```haskell
parseLists :: String -> ([Int],[Int])
parseLists input = unzip (map parseLine (lines input))
```

Then we are going to use `$` and `<$>`.

You can read `f $ b` as simply `f (b)` , the magic is that `b` can be something complex and `$`
allow us to omit the parenthesis.

And for `<$>` it is just a way to use `map` as an operator, the code `a <$> b` is just `map a b`

This allows us to write:

\begin{code}
parseLists :: String -> ([Int],[Int])
parseLists input = unzip $ parseLine <$> lines input
\end{code}


Solving the problem
-----

The problem can be solved by just:
- sort the lists from the lowest item to the biggest one.
- Take the sorted lists item by item and construct the list of differences.
- Sum all the differences list.

For the first part we have the function `sort` already available.

```haskell
let
  leftListSorted = sort leftList
  rightListSorted = sort rightList
```
For the second part we want to iterate both list at the same time, so we use the `zip`

\begin{code}
listDistance :: [Int] -> [Int] -> Int
listDistance l r =
  let lsort = sort l
      rsort = sort r
  in
    sum $  (\ (x,y) -> abs (x-y) ) <$> zip lsort rsort

solve1 :: IO ()
solve1 = do
  content <- readFile "AdventOfCode/Data/2024/1.txt"
  let (l,r) = parseLists content
  let distance = listDistance l r
  -- the solution for my input! 1319616
  print distance


countFrequencies :: [Int] -> IntMap Int
countFrequencies input = IntMap.fromListWith (+) ((\ x -> (x,1)) <$> input)

sumList ::  IntMap Int-> [Int]-> Int
sumList frequences =
  foldr
    (\ newValue acc -> acc + newValue * ( fromMaybe 0 (IntMap.lookup newValue frequences)))
    0

solve2 = do
  content <- readFile "AdventOfCode/Data/2024/1.txt"
  let (l,r) = parseLists content
  let frequences = countFrequencies r
  let result = sumList frequences l
  -- the solution for my input! 27267728
  print result
\end{code}
