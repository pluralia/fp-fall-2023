# Haskell Fall 2023

Haskell course homework

## How to pass the course

- homework
    - after each lecture | **10 points max**
    - deadline: Thursday 23:59 | **0 points per task after the deadline**
    - 1 week to fix my comments | **all comments fixed -> full grade**.
    - write tests | **no tests -> your grade goes down**
    - project can be built and tests work | **build/tests fail to my review -> -2 points**
        - write me before opening PR if you can't solve the problem
    - use `hlint` and `-Wall` | **not explained compiler warnings and hlint recommendations -> -1 point**
    - do not pass solutions to each other | **explicit reuse -> 0 points per task**
        - write me before opening PR if you struggle with the task and want to get a hint
    - Once a semester, you may pass homework after the deadline (no more than 1 week later) and get a full grade
- exam
    - theory + practice
- [grade for the course] = [average homework grade] * 0.5 + [exam] * 0.5

### How to pass your homework

1. Fork this repo -- each branch contains the homework for the corresponding lecture
2. Create a branch `hw<x>` and your project directory `hw<x>`, where ```x `elem` [1..12]```
3. As you complete the assignment
   - check you have no compiler warnings and all tests pass: `cabal clean && cabal test`
   - all hlint hints are fixed: `hlint .`
   - open the PR in this repo
   - the PR should be named: <HW###>, <LastName_FirstName>
4. I review your PR and leave my comments: accepted or requires fixes

---

# Program

## 1 lecture

#### Introduction

- Motivation for learning Haskell
- FP core concepts
    - immutability
    - recursion
    - high-order functions
    - pattern matching
    - currying
- Haskell core concepts
    - Functional
    - Statically typed
    - Pure
    - Type inference
    - Concurrent
    - Lazy
    - Packages
- Comparison with other languages
- Motivation to use Haskell
    - Memory safety
    - High performance
    - Bugs are caught early & Explicit data model
    - Correct software
    - Ease of refactoring & Composability
    - Well-developed ecosystem
- What to read about Haskell
- History

### Basics I

- Terms & their properties
    - associativity
    - binding order is unimportant
    - immutability
    - lazy by default
- Binding
    - global & local
    - `let`, `where`
- Functions (+ lambdas)
- Pattern-matching (+ `case of`)
- Branching
    - `if-then-else`
    - guards
- Operators
- Currying
- Recursion & Tail recursion
- Types
    - Base types (`Bool`, `Char`, `Int`, `Integer`, `Float`, `Double`, `T1 -> T2`, `(T1, T2, ..., Tn)`, `()`, `[T]`)
    - Type constraints
    - Custom types
    - Standard module types (`Maybe`, `Either`, list, tuple)


## 2 lecture

#### Environment and project

- GHC & GHCi
- Packages и Hackage
- Build Tools (stack & cabal)
- Project creation and its structure
- Haskell Language Server (HLS)
- GHCup

#### Code

- HLint
- Haskell API search engine `hoogle`
- Standard module `Prelude`
- HSpec
- Pragmas
    - *TupleSections*
    - *LambdaCase*

#### Basics II

- list comprehension
- `foldr` / `foldl` / `foldl1`

---

## 3 lecture

#### Type Classes

- Type class
- Polymoprhism
    - Parametric
    - Ad-hoc
- Class instance `instance`
- Standard type classes (`Eq`, `Ord`, `Num`, `Show`, `Read`)
- `deriving`
- Type classes for high order types
- Type classes are syntax sugar

#### Data Types

- ADT
- Records
- Pragmas
    - *RecordWildCards*
- `data` vs `newtype` vs `type`

#### Point-free style

- Operators
    - function application `$`
    - reversed function application `&`
    - function composition `.`

---

## 4 lecture

#### List-like data structures

- What the lists are in reality
- Other types
    - `Text`
    - `String`
    - `Vector`
    - `Set`
    - `Map`
- `foldl` / `foldr` for other types

#### Haskell in BIOCAD

---

## 5 lecture

#### `Monoid` and friends

- `Magma`
- `Semigroup`
- `Monoid`

#### `Foldable`

#### `Functor`

---

## 6 lecture

#### Functors

- `Functor`
- `Applicative`
- The meaning of `f <$> x <*> y <*> z`

#### Applicative parsers I

- Parser definition
    ```haskell
    newtype Parser tok a 
        = Parser { runParser :: [tok] -> Maybe (a, [tok])}
    ```
- `Functor` for `Parser`
- `Applicative` for `Parser`
- Examples of simple parsers

---

## 7 lecture

#### Applicative parsers II

- `Alternative` for `Parser`
- Parser for CSV 

#### Monads I

- Random generation in Haskell
- `Monad`
- Evaluation context
  - Maybe, List, Either, Reader, Writer
  - Kleisli arrow

---

## 8 lecture

#### Monads II

- Генератор псевдослучайных чисел
- `Traversable`
- `Identity` monad
- do-notation
- `MonadFail`
- `Maybe`, `[]`
- `Writer`, `Reader`

---

## 9 lecture

### Monads III

- `State`
- `IO`

### Monad Transformers

---
