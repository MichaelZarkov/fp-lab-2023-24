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

module Lazy where

import Prelude hiding (cycle, foldl, foldr, repeat, scanl)

-- boolean blindness
-- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

-- parseNumber :: String -> Maybe Integer
-- isNumber :: String -> Bool

-- case parseNumber "asdf" of
--   Nothing -> ...
--   Just num -> num

-- convertToNumber :: String -> Int
--
--
-- if isNumber "asdf"
-- then
--   convertToNumber "asdf"
-- else ...

-- (parseNumber "asdf")
-- (if ....

-- filter

-- import Data.Maybe
-- mapMaybe :: (a -> Maybe b) -> [a] -> [b]
-- filter :: (a -> Bool) -> [a] -> [a]

-- thunk -- delayed computation, replaced when done
-- :print, :sprint -- don't forget type annos -- sometimes unreliable, different results between ghc versions
-- Nat, isZero, some tuple stuff?

data Animal = Dog Integer | Cat String
  deriving (Show)

-- weak head normal form
-- WHNF
isDog :: Animal -> Bool
isDog x =
  case x of
    Dog _ -> True
    Cat _ -> False

-- or' :: Bool -> Bool -> Bool
-- or' False False = False
-- or' _ _ = True

-- or' x y =
--  case x of
--    False ->
--      case y of
--        False -> False
--        _ -> True
--    _ -> True

-- when do we "need" to evalute something? case matches!
-- how much do we need to evaluate it? WHNF

-- infinite structures
-- Stream, but we will use [] with asserts*
-- error
-- show take def?

-- Debug.Trace

-- foldr, foldl, stack space
--
-- @seq :: a -> b -> b@
-- @seq a b@ - "evaluate a and b together" - no enforced order, but usually used to mean a before b
--  @seq a b@ terminates iff a terminates
--
-- very detailed SO answer describing seq in more detail:
-- https://stackoverflow.com/a/66965677
--
-- A note on evaluation order: the expression seq a b does not guarantee that a will be evaluated before b.
-- The only guarantee given by seq is that the both a and b will be evaluated before seq returns a value.
-- In particular, this means that b *may* be evaluated before a.
-- It also means that if you entirely ignore the result of @seq a b@, neither @a@ nor @b@ will be evaluated,
-- for example in @const 5 (seq undefined undefined)@.
--
-- foldl' with seq and bang
-- mention deepseq

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x : xs) = foldl f (f acc x) xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f !acc (x : xs) = foldl' f (f acc x) xs

-- WHNF

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ nv [] = nv
foldr f nv (x : xs) = x `f` foldr f nv xs

-- seq the acc
-- bang the acc

-- EXERCISE
-- Infinitely repeat a value
-- >>> take 4 $ repeat 'a'
-- "aaaa"
repeat :: a -> [a]
repeat x = x : repeat x

-- EXERCISE
-- A list of all the natural numbers.
-- EXAMPLES
-- >>> take 10 nats
-- [0,1,2,3,4,5,6,7,8,9]
nats :: [Integer]
nats = 0 : map succ nats

-- fix :: (a -> a) -> a
-- fix f = f (fix f)

-- [0, succ 0, succ (succ 0), succ (succ (succ 0))
-- 0 : map succ (0 : map succ (0 : ....))

-- EXERCISE
-- Generate an infinite list of numbers, starting with the given number, with the given interval between each numbe.
-- EXAMPLES
-- >>> take 10 $ fromThen 0 1
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> take 20 $ fromThen 0 1
-- [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
-- >>> take 10 $ fromThen 4 9
-- [4,13,22,31,40,49,58,67,76,85]
-- >>> take 10 $ fromThen 0 (-10)
-- [0,-10,-20,-30,-40,-50,-60,-70,-80,-90]
fromThen :: Integer -> Integer -> [Integer]
fromThen a b = a : fromThen (a + b) b

-- EXERCISE
-- Implement a list of all the factorial numbers
-- Use a where or let to "cache" the current number, so we don't do all the multiplications every time.
-- i.e. if we've already calculated 5!, we can simply multiply the result by 6, we don't need to calculate 5! again.
-- EXAMPLES
-- >>> take 10 facts
-- [1,1,2,6,24,120,720,5040,40320,362880]
facts :: [Integer]
-- add a 1 to account for fact 0
facts = go 1 1
  where
    -- invariant: prevRes holds the factorial of (n - 1)
    go n prevRes = prevRes : go (succ n) (prevRes * n)

-- EXERCISE
-- "Caching foldl"
-- It's sometimes useful to have all the "intermediate" results of a fold. It's also helpful for debugging sometimes.
-- Implement a version of foldl that returns all of the intermediate results it has.
-- These are called "scans" in the Haskell standard library.
-- EXAMPLES
-- >>> scanl (+) 0 [1..10]
-- [0,1,3,6,10,15,21,28,36,45,55]
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _f acc [] = [acc]
scanl f acc (x : xs) = acc : scanl f (f acc x) xs

-- EXERCISE
-- Use scanl to implement facts.
-- EXAMPLES
-- >>> take 10 factsScanl
-- [1,1,2,6,24,120,720,5040,40320,362880,3628800]
factsScanl :: [Integer]
factsScanl = scanl (*) 1 $ drop 1 nats

-- EXERCISE
-- Implement a list of all the fibonacci numbers.
-- Use the following idea:
-- The fibonacci numbers start with 0 1
-- To generate the next fibonacci number, we need to sum the previous two, so assuming we already have
-- fibs :: [Integer]
-- that would mean summing the head of fibs with the head of the tail of fibs
-- zipWith will be useful here.
-- EXAMPLES
-- >>> take 10 fibs
-- [0,1,1,2,3,5,8,13,21,34]
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

-- EXERCISE
-- Idea:
-- We can get the all the primes with the following algorithm.
-- Let's start with all the numbers >=2
-- The current number - call it x - is prime (this is true in the beginning - x = 2 is prime) - leave it in our list
-- All of the other numbers that are divisible by x aren't prime - filter them out
-- This is called the sieve of Eratosthenes
-- Implement this for a given input list
-- Now all we need to do is apply it to all the natural numbers >=2 to get a list of all the primes.
-- EXAMPLES
-- >>> take 10 $ primes
-- [2,3,5,7,11,13,17,19,23,29]
-- >>> take 20 $ primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71]
primes :: [Integer]
primes = eratosthenes $ drop 2 nats
  where
    eratosthenes :: [Integer] -> [Integer]
    eratosthenes (p : xs) = p : eratosthenes (filter (not . divides p) xs)
    eratosthenes [] = error "shouldn't happen"

    divides x y = y `rem` x == 0

-- EXERCISE
-- Infinitely repeat a list
-- >>> take 7 $ cycle [1,2,3]
-- [1,2,3,1,2,3,1]
cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs

-- Let's consider the following problem:
-- We have a "circle" of n people. We have an integer k.
-- We repeat the following procedure, starting from the first person, until there is only one left:
--
-- "Kill" the kth person. Repeat this, "starting" from the k+1th person, i.e. we start indexing from the k+1th person.
--
-- Example execution for n = 5, k = 2:
-- 1 2 3 4 5 (start)
-- 1 2 4 5 (we kill 3, since 0 + 2 = 2)
-- 2 4 5 (we kill 1, since 4 + 2 = 6, circling back to 1)
-- 2 4
-- 4
--
-- The question is, given arguments n and k, what's the number of the final person left. (4 in the example above)
--
-- This is called the josephus problem - see https://en.wikipedia.org/wiki/Josephus_problem.
--
-- Your task is to implement @jos :: Integer -> Int -> Integer@
-- which takes n and k, and returns the last surviving person.
--
-- Your hint is to use `cycle` to express our circle, and then to drop and filter things from it.
-- E.g. the representation of our circle for n = 5 would be [0,1,2,3,4,0,1,2,3,4,0,1,2,3,4...]
-- EXAMPLES
-- >>> jos 5 2
-- 4
-- >>> jos 10 4
-- 3
-- >>> jos 10 8
-- 7
-- >>> map (\(x,y) -> (x, y, jos x y)) [(x, y) | x <- [2..5], y <- [2..5]]
-- [(2,2,2),(2,3,1),(2,4,2),(2,5,1),(3,2,2),(3,3,2),(3,4,1),(3,5,1),(4,2,1),(4,3,2),(4,4,2),(4,5,3),(5,2,4),(5,3,1),(5,4,2),(5,5,4)]
jos :: Integer -> Int -> Integer
jos n k = untilJust isDone go $ cycle [1 .. n]
  where
    -- figure out what this function should do based only on the types and the name
    -- I think there's only one valid type safe implementation of this
    -- ask me if you're confused
    -- this function exists in base but with Bool instead of Maybe
    untilJust :: (a -> Maybe b) -> (a -> a) -> a -> b
    untilJust p f x =
      case p x of
        Nothing -> untilJust p f (f x)
        Just res -> res
    -- the procedure which actually does the removal
    -- since our circle is expressed as an infinite list
    -- to remove a person, we need to drop them from the head of the list
    -- and then remove all their other occurrences
    go :: [Integer] -> [Integer]
    go xs =
      case drop k xs of
        [] -> error "shouldn't happen"
        dead : survivors -> filter (/= dead) survivors
    -- since we're expressing our circle via an infinite list in which we remove
    -- all of the encounters of a single person to remove them from the circle
    -- that means that when there's a single person left in the circle, we should have
    -- the same element repeating indefinitely, which we can detect by looking at two adjacent list elements
    isDone :: [Integer] -> Maybe Integer
    isDone [] = error "shouldn't happen"
    isDone (x : y : _) | x == y = Just x
    isDone _ = Nothing
