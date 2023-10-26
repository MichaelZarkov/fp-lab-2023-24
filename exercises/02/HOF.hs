{-# LANGUAGE RankNTypes #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

module HOF where

import Prelude hiding (const, curry, id, log, on, swap, uncurry, until, ($))

-- |
-- remind about hw deadline
-- say soon hw2?
--
-- talk about
-- @-bindings
-- guards
-- let
-- where
-- blaswap :: (Int, Char) -> (Char, Int)
-- blaswap asdf@(x, y) =
--  let f' = 11 * x
--   in (let bla = y in bla, f + f')
--  where
--    f = x + 10
andBool :: Bool -> Bool -> Bool
andBool False _ = False
andBool True False = False
andBool True True = True

-- \x y z -> x * y + z
-- \x -> \y -> \z -> x * y + z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f $ f x

-- logGeneral ::
--  (String -> IO ()) ->
--  String ->
--  String ->
--  IO ()
-- logGeneral prnt lvl str =
--  prnt (lvl ++ ":" ++ str)
--
-- logOut :: String -> String -> IO ()
-- logOut lvl str = logGeneral putStrLn lvl str
--
-- logFile :: String -> String -> IO ()
-- logFile lvl str = logGeneral (writeFile "asdf.txt") lvl str

-- f x y z = x * y + z
-- f = \x y z -> x * y + z

-- mapmap f = map (map f)

add5 :: Int -> Int
add5 = (+) 5

-- f :: a -> Int -> Char -> String
-- x :: a
-- f x :: Int -> Char -> String
--
-- (+) :: Int -> (Int -> Int)
-- (+) 5 :: Int -> Int

-- ?
-- lambdas, desguar function def, HOF(arguments), log hof example, currying, polymorphism
-- logging as example for DI via higher order functions
-- ($)
-- what does "combinator" mean?
--
-- TODO: implement
-- applyTwice :: (a -> a) -> a -> a
-- id
-- ($) - maybe go back to solutions and start rewriting stuff using ($)?

($) :: (a -> b) -> a -> b
($) f x = f x

infixr 0 $

-- x + y + z + u

-- f $ g $ y x

-- 3 + (5 * 10) + 6

todo :: todo
todo = todo

data Tuple a b = MkTuple a b

sumTuple :: Tuple Int Int -> Int
sumTuple (MkTuple x y) = x + y

-- EXERCISE
-- Take two arguments and return the first.
-- This is called const because if we think of it as a function
-- on one argument x, it returns a function that when called, always returns x
-- It is also practically always used partially applied.
-- EXAMPLES
-- >>> const 5 6
-- 5
-- >>> applyTwice (const 42) 1337
-- 42
const :: a -> b -> a
const = undefined

-- EXERCISE
-- Compose two functions, very useful very often
-- there's a builtin (.) for this - the dot mimics mathematical notation f o g
-- EXAMPLES
-- >>> let f = compose (+3) (*5) in f 4
-- 23
-- >>> let f = compose (*5) (+5) in f 4
-- 45
compose :: (b -> c) -> (a -> b) -> a -> c
compose = undefined

-- EXERCISE
-- Iterate a function f n times over a base value x.
-- EXAMPLES
-- >>> iterateN (+1) 1 10
-- 11
-- >>> iterateN (*2) 1 10
-- 1024
iterateN :: (a -> a) -> a -> Integer -> a
iterateN = undefined

-- EXERCISE
-- Swap the two elements of a tuple
-- EXAMPLES
-- >>> swap $ MkTuple 42 69
-- MkTuple 69 42
swap :: Tuple a b -> Tuple b a
swap = undefined

-- EXERCISE
-- Apply a function to only the first component of a tuple
-- EXAMPLES
-- >>> first (*2) $ MkTuple 21 1337
-- MkTuple 42 1337
first :: (a -> b) -> Tuple a c -> Tuple b c
first = undefined

-- EXERCISE
-- Convert a function operating on a tuple, to one that takes two arguments.
-- Called Curry after Haskell Curry - inventor of lambda calculus.
-- EXAMPLES
-- >>> curry (\(MkTuple x y) -> x * y) 23 3
-- 69
curry :: (Tuple a b -> c) -> a -> b -> c
curry = undefined

-- EXERCISE
-- Convert a two argument function, to one that takes a Tuple.
-- EXAMPLES
-- >>> uncurry (\x y -> x + y) $ MkTuple 23 46
-- 69
uncurry :: (a -> b -> c) -> Tuple a b -> c
uncurry = undefined

-- EXERCISE
-- > p `on` f
-- Implement a combinator that allows you to "preapply" a function f on the arguments of a function p
-- EXAMPLES
-- >>> let maxOnFirst = max `on` fstTuple in maxFirst (MkTuple 1 20) (MkTuple 2 100000)
-- 2

-- >>> let maxOnSum = max `on` sumTuple in maxOnSum (MkTuple 20 39) (MkTuple 12 34)
-- 59
-- You can look at the `fight` from the solutions from last time for good actual usage of the function
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on = undefined

-- | EXERCISE
-- Execute a function, until the result starts sastifying a given predicate
-- EXAMPLES
-- >>> until (>1000) (*7) 4
-- 1372
until :: (a -> Bool) -> (a -> a) -> a -> a
until = undefined

-- EXERCISE
-- Apply two different functions to the two different arguments of a tuple
-- Think about what the type should be.
-- mapTuple :: ???
-- mapTuple = undefined

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

-- EXERCISE
-- Look at addNat and multNat from last time.
--
-- addNat :: Nat -> Nat -> Nat
-- addNat Zero m = m
-- addNat (Succ n) m = Succ (addNat n m)
--
-- multNat :: Nat -> Nat -> Nat
-- multNat Zero _ = Zero
-- multNat (Succ n) m = addNat m (multNat n m)
--
-- They look very similar.
-- Can you implement a general enough higher-order function (called foldNat here), such that you can then use to
-- implement both of addNat and multNat by passing suitable arguments? What are those arguments?
--
-- foldNat :: ???
-- foldNat = ???
--
-- If your function is "good enough" you should also be able to implement exponentiation using it.
-- expNat :: Nat -> Nat -> Nat
-- expNat = undefined
--
-- Can you also implement the following "predecessor" function using it? Yes/no, and why?
--
-- predNat :: Nat -> Nat
-- predNat Zero = Zero
-- predNat (Succ n) = n
--
-- If not, can you think of a foldNat' :: ??? with a different type signature, which would allow you to implement predNat?
