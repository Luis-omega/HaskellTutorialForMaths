------
title: Setting up a simple Haskell project using Cabal.
date: 2024-11-17
tags: Tutorial, advent of code, 2024, setup, cabal
------

A mini tutorial about how to use cabal to configure a new project.

I'm going to write a series solving the [Advent Of Code 2024](https://adventofcode.com/2024)
problems and this blog entry will focus on how to configure a simple
project using the **cabal** tool.

Assuming you already have **cabal** available to you (otherwise you can [install
it](https://www.haskell.org/cabal/) in multiple ways).

Cabal init.
------

Our first step is to create a new folder `AdventOfCode`, enter inside
it and run `cabal init` with some options.
(We assume you are using *nix operating system or wls in windows)

```bash
mkdir AdventOfCode
cd AdventOfCode
cabal init --libandexe --tests
```

In this case we choose the options `--libandexe --tests`, this mean that
cabal would generate a project structure with tree things in mind:

- Library code: This is code that we can distribute as a library and
  is not intended to directly generate an executable that you can run.
  This is the code that is checked when you run `cabal build`
- Executable code: This is code that can use multiple libraries and
  cabal would construct an executable for it.
  The executable can be run using `cabal run`, however note that
  if you configure multiple executables on the cabal file (yes you can)
  then you need the specify the name of the executable to run.
- Test code: Code that won't be included as part of the library or
  the executable and is used to run test on the code.
  This can be executed with `cabal test`.

This is the tree structure that the above command would generate:

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

The file `AdventOfCode.cabal` contains the cabal configuration for the project.
Is inside this file were you must add more files to the library/executable/tests
for cabal and ghc to include them at compilation time.

With this you should be able to run the tree commands:

```bash
cabal build
cabal run
cabal test
```

They shouldn't do much, but they also shouldn't fail. If there is a error at
this point you need to solve it before advancing further!

If you open the file `AdventOfCode.cabal` after the initial section with
the project information, you can see a section like this:

```haskell
library
    exposed-modules:  MyLib

    -- Modules included in this library but not exported.
    -- other-modules:

    -- LANGUAGE extensions used by modules in this package.
    -- other-extensions:
    build-depends:    base ^>=4.14.3.0
    hs-source-dirs:   src
    default-language: Haskell2010
```

You can refer to the [cabal documentation](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html)
for information about those and more fields, but let's have an overview of them:

- The **exposed-modules** field are the modules (files as every file is a module)
  that other people can import when they add our library as a dependence.
  In this case cabal automatically created a `src/Mylib.hs` file and added it
  as the only export for now.
- The **other-modules** field is for modules that are part of the library
  but that other people can't use when they import the library. This mean
  that only the code inside the library can use them. This would be useful
  if you want to hide the details of how you implement something and only
  expose an interface in other modules of the library.
  To use this field you need to delete the **--** before it (i.e. uncomment the
  line).
- The **build-depends** is where you would put other libraries that your library
  depend on. You can add local libraries (i.e. libraries that you have in
  other parts of your computer) or libraries from some source of libraries.
  The default source of libraries for cabal is [Hackage](https://hackage.haskell.org/)).
  In this case we have the **base** library added as a dependence, it has
  a lot of basic types and functions that you usually want access while you
  are learning (like Int, List, Tuple, Maybe, IO, etc).
  The numbers after the name base are for cabal to choose a version of base
  that can work with our code. I'm not gonna lie, this is going to be a
  source of headaches for you if you have lots of dependencies in the future
  in any project. As such, we are not going to talk more about this part
  in this tutorial (sorry).
- The **hs-source-dirs** is a path to the folder that contains all the code
  of our library. If you want to add a new module to **exposed-modules**
  you need to put the file under this folder.

Then in the next section you should see something like:

```haskell
executable AdventOfCode
    main-is:          Main.hs

    -- Modules included in this executable, other than Main.
    -- other-modules:

    -- LANGUAGE extensions used by modules in this package.
    -- other-extensions:
    build-depends:
        base ^>=4.14.3.0,
        AdventOfCode

    hs-source-dirs:   app
    default-language: Haskell2010
```

This section defines a executable whose name is **AdventOfCode**.
It means that we can use `cabal run AdventOfCode` to run this executable.

We only see a new field here:

The **main-is** field is to specify the name of the module that contains a
**main** function. This **main** function is the function that we are going
to run when we do `cabal run`. It is an error if it's not present. In
our case it is setted to **Main.hs**, as we also have **hs-source-dirs**
setted to **app**, this means that cabal expects to see the **Main.hs**
file in the path **app/Main.hs**.

You may also notice the that **build-depends** has **base** (as before)
and **AdventOfCode**. The meaning of this is that our library
defined above is a dependence of our executable. This is, the
executable can see all the exported modules of our library and
use all the exported functions and types from it.


Finally, the test section.

```haskell
test-suite AdventOfCode-test
    default-language: Haskell2010
    type:             exitcode-stdio-1.0

    -- Directories containing source files.
    -- hs-source-dirs:
    main-is:          MyLibTest.hs
    build-depends:    base ^>=4.14.3.0
```

In this case you should notice that it didn't specify a **hs-source-dirs**,
but it has a **main-is** and that cabal generated a file **MyLibTest.hs**
at the root of the project. Usually you want to set the **hs-source-dirs**
to something like **tests** and put the main file inside this folder.

The **build-depends** is for adding new libraries that your original library
and executable may not need but that can simplify your life to write tests!
This means that the users of this library won't need those libraries whenever
they use your library unless they want to run your library tests.


Modifying the default configuration
----

We are going to change the default structure of our project from:

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

To:

```bash
.
├── AdventOfCode.cabal
├── app
│   └── Main.hs
├── CHANGELOG.md
├── tests/Main.hs
└── src
    └── Problem1.hs
```

And make the changes inside the files to reflect this change.

First we create the **tests** folder and move the **MyLibTest.hs** to it.

At the root of the project

```bash
mkdir tests
mv MyLibTest.hs  tests/Main.hs
```

Then we change the name of **src/Mylib.hs** to **src/Problem1.hs**

```bash
mv src/MyLib.hs  src/Problem1.hs
```

Now to tell cabal that we want those changes we need to update the fields.

The **library** section has to change the **exposed-modules** like this:

```haskell
    exposed-modules:  Problem1
```

The **test** section needs to uncomment the **hs-source-dirs** and the section
like:

```haskell
    hs-source-dirs:  tests
    main-is: Main
    build-depends:    base ^>=4.14.3.0
    ,AdventOfCode

```

Now if you try any of the commands of cabal (**run**/**build**/**test**)
you would find that all of them fail!

For example `cabal build` gave me:

```bash
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - AdventOfCode-0.1.0.0 (lib) (configuration changed)
 - AdventOfCode-0.1.0.0 (exe:AdventOfCode) (configuration changed)
Configuring library for AdventOfCode-0.1.0.0..
Preprocessing library for AdventOfCode-0.1.0.0..
Building library for AdventOfCode-0.1.0.0..

src/Problem1.hs:1:8: error:
    File name does not match module name:
    Saw: ‘MyLib’
    Expected: ‘Problem1’
  |
1 | module MyLib (someFunc) where
  |        ^^^^^
cabal: Failed to build AdventOfCode-0.1.0.0 (which is required by
exe:AdventOfCode from AdventOfCode-0.1.0.0).
```

We need to go to **src/Problem1.hs** and change the first line from:

```haskell
module MyLib (someFunc) where
```

To:

```haskell
module Problem1 (someFunc) where
```

Every file in Haskell is a module and the module we are declaring
in this line must match the name of the file (or the path, more on that later)


If you try again the error changes to

```haskell
app/Main.hs:3:1: error:
    Could not find module ‘MyLib’
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
3 | import qualified MyLib (someFunc)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

This is because the **app/Main.hs** file is importing the `someFunc`
function from the old file **src/MyLib**, we need to change this.

If you open **app/Main.hs** it may look like

```haskell
module Main where

import qualified MyLib (someFunc)

main :: IO ()
main = do
  Mylib.someFunc
```

We need to replace the `MyLib` for `Problem1` in every place we
find it, then we are going to see something like this:

```haskell
module Main where

import qualified Problem1 (someFunc)

main :: IO ()
main = do
  Problem1.someFunc
```

After this there shouldn't be more problems using **cabal build** or **cabal
run**. However, you still need to change the occurrences of `Mylib` inside
**tests/Main.hs** after that you shouldn't have issues with
**cabal test**.

The problem's module structure.
----

The problems of advent of code always have two parts, to reflect this
every problem we solve will have two functions: `solve1` and `solve2`.
The `solve1` function would solve the original problem and the
`solve2` the modified one unblocked after solving the first problem.

However during the series we want to have two additional functions:
`solve1_2` and `solve2_2`. They also correspond to solutions of the first and
second part of a problem, but they are intended to be for experimentation
in the blogs.

If `solve1` used a very naive way to do something then `solve1_2` may use
some advance technique that we are going to explore. But usually the
functions suffixed with `_2` will be used to explore how we should
write the code assuming that we are in a big project with a team. This
way you can begin to learn how to write code that others can read at work.


With this said we need to modify **src/Problem1.hs** like this:

```haskell
module Problem1(solve1,solve1_2,solve2,solve2_2) where

solve1 :: IO ()
solve1 = undefined

solve1_2 :: IO ()
solve1_2 = undefined

solve2_1 :: IO ()
solve2_1 = undefined

solve2_2 :: IO ()
solve2_2 = undefined
```

Then under **app/Main.hs** we need to change the file like

```haskell
module Main where

import qualified Problem1 (solve1,solve1_2,solve2_1,solve2_2) qualified as P1

main :: IO ()
main = do
  P1.solve1
```

Every time you want to run a particular solver, you only need to change the

```haskell
main = do
  P1.solve1
```

To the appropriate solver.

Every time you want to add a new problem you need to crate a
new **src/ProblemN.hl** with the same functions and
add the import to **app/Main.hs**.

And that's it! We are ready to began to solve the advent of code
problems!

Note that we didn't update the **tests/Main.hs** file, this mean that our test
are broken, but we are not going to use them anyway, we are
going to keep them broken unless we need them later in the series. We
only included them to give a complete guide on how to configure a
project!


From here you can:

- Read other fields that you can use in your cabal file in the [cabal documentation](https://cabal.readthedocs.io/en/stable/cabal-package-description-file.html).
- Experiment adding dependencies to your projects (like the `containers` or
  `mtl` libraries). You eventually will hit a wall if you do this, but you
  need to do it in order to learn how to jump this wall (and then hit it
  again in other ways with more advanced tools…).
- Attempt to solve the AOC problems (we did all this for that reason after all).
- Use this as a template for new projects (well you can also have project
  templates that cabal can use to fill things in the `init` face, but I didn't
  mean this!).

The blog real structure of the project
---

Although this blog has a Cabal file it doesn't follow completely the
structure described here. Not only that but we also have
a **nix-flake** file that we use to manage the project together with **cabal**.
The use of nix mitigated some of the problems that Haskell had for
a long time with dependencies (up to some degree). But some times
you still have those problems.

I plan to write another entry where I talk about the particular
configuration of the blog, but for now this is what you may want to
know about the current [cabal file](https://github.com/Luis-omega/Learning-Haskell/blob/f3e7eb8c8d671228b4636d921c63e88e6e9a8d66/Blog.cabal).

- I have **library** section pointing to a **posts** folder.
  Here I write things in either markdown or literate Haskell.
  I put the files under a library for the **hslp** to accept them as
  part of the project!
- I have a **site** executable that generated all the html
  code that you can see at [my blog](https://luis-omega.github.io/Learning-Haskell/).
  I'm using the Hakyll library to create this.
  This also means that I can create a pdf version of
  every entry with ease, but I don't want to share them for now,
  if you are interested you are free to do it yourself!.
  I try hard to use only features that can be properly handled by
  html and latex in the blog.
- I have a dedicated **aoc** executable.
  Instead of putting all the code in a library and use it in the
  executable I put them in the **other-modules** as directly part of the
  **aoc** executable.
  I have to do this since the regular blog entries already are a
  library and from what I know cabal can't handle having more than
  one library.
  There are other solutions like opening another repo or using
  another project, but I wanted AOC to be part of the blog project,
  so I choose this for now.
  I also needed **hslp** linting in those files and this is a
  way to have it.

I'm just at the beginning of writing the AOC2024 entries, but I think I
would need to add a tests section for the problems (as sometimes I
need to test some functions). But I'm still not convinced of the benefit of it.
