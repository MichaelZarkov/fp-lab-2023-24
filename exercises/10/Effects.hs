{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoStarIsType #-}
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

module Effects where

import Control.Applicative (Applicative (liftA2, pure))
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Prelude hiding (traverse, (*>), (<*), (<*>))

-- TODO
-- mention exam form

-- TODO kinds
-- mention kinds

-- Maybe :: Type -> Type
-- Maybe :: * -> *

-- TODO
-- functor

-- map ::       (a -> b) -> [] a      -> [] b
-- maybeMap ::  (a -> b) -> Maybe a  -> Maybe b
-- mapIO ::     (a -> b) -> IO a     -> IO b
-- parserMap :: (a -> b) -> Parser a -> Parser b

-- fmap :: (a -> b) -> f a -> f b
-- f :: Type -> Type

-- >>> incr $ Just 5
-- Just 6
-- >>> incr $ [1..10]
-- [2,3,4,5,6,7,8,9,10,11]
-- >>> incr $ readLn
incr :: (MyFunctor f) => f Int -> f Int
incr = myFmap (+ 1)

-- fmap :: (a -> b) -> f a -> f b
-- fmap id x === x
-- fmap id === id
-- fmap f . fmap g === fmap (f . g)
class MyFunctor (f :: Type -> Type) where
  myFmap :: (a -> b) -> f a -> f b

instance MyFunctor [] where
  myFmap = map

instance MyFunctor Maybe where
  myFmap _ Nothing = Nothing
  myFmap f (Just x) = Just $ f x

instance MyFunctor IO where
  myFmap f mx = do
    x <- mx
    pure $ f x

data Dog = MkDog
  { name :: String,
    age :: Int,
    breed :: Breed
  }
  deriving (Show, Read)

data Breed = Husky | GreatDane
  deriving (Show, Read)

-- (<&>) :: f a -> (a -> b) -> f b
-- >>> :t readMaybe
-- readMaybe :: Read a => String -> Maybe a
parseDog :: String -> String -> String -> Maybe Dog
parseDog name ageString breedString = do
  age <- readMaybe ageString
  breed <- readMaybe breedString
  Just $ MkDog {name, age, breed}

-- >>> bla
-- [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
-- [(x, y) | x <- [1..3], y <- [4..6]]
bla = do
  x <- [1 .. 3]
  y <- [4 .. 6]
  pure (x, y)

readTwoAndSumAndPrint :: IO ()
readTwoAndSumAndPrint = do
  (x :: Int) <- readLn
  y <- readLn
  print $ x + y

-- newtype Parser a

-- instance Monad Parser where...

class (MyFunctor f) => MyApplicative f where
  -- Just :: a -> Maybe a
  -- pure :: a -> Maybe a
  -- pure :: a -> IO a
  -- (:[]) :: a -> [a]
  myPure :: a -> f a
  myLiftA2 :: (a -> b -> c) -> f a -> f b -> f c

class (MyApplicative m) => MyMonad m where
  (<$>>) :: m a -> (a -> m b) -> m b
-- readTwoAndSumAndPrint :: IO ()
-- readTwoAndSumAndPrint =
--   readLn >>= \(x :: Int) ->
--   readLn >>= \y ->
--     print $ x + y

-- (>>=) :: m a -> (a -> m b) -> m b
-- join :: m (m a) -> m a
-- (>>=) => join
-- join+fmap => (>>=)
-- (>>=)

--  readMaybe ageString >>= \age ->
--  readMaybe breedString >>= \breed ->
--    Just
--      MkDog
--        { name,
--          age,
--          breed
--        }

-- ^ f :: a -> Maybe b

-- MkDog name (readMaybe ageString) $ readMaybe breedString

-- ($) :: (a -> Maybe b) -> Maybe a -> Maybe b

-- readMaybe breedString :: Maybe a

-- readMaybe breedString :: Maybe Breed
-- f ==> (a -> b) ~ Breed -> Maybe Dog
-- f ==> a ~ Breed

-- readMaybe breedString :: Maybe Breed
-- a ~ Maybe Breed

-- ($) :: (a -> b) -> a -> b

-- TODO
-- bind as "effectful application"
-- or kleisli arrows as "effectful composition"
-- why do we need them in the first place?
-- parsing a data type from many strings?
-- get from IO+Parser to bind
-- show fmap and liftA2 from bind

-- TODO
-- do via bind

-- TODO
-- for more info on parsers (implementation and origin):
-- https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf

-- TODO
-- type classes - https://wiki.haskell.org/Typeclassopedia

-- TODO
-- explain
newtype State s a = MkState {run :: s -> (s, a)}

incr' :: State Int ()
incr' = MkState $ \n -> (n + 1, ())

incr'' :: State Int ()
incr'' = do
  x <- get
  put $ x + 1

-- instance Monad State

-- (>>=) :: State s a -> (a -> State s b) -> State s b
-- myWordle = do
--   currentWords <- get
--   if match currentWords targetWord
--   then do
--     pure Winner!
--   else do
--   put $ guess : currentWords
--   myWordle

-- f :: [String] -> .... -> ([String], a)

-- f ::

-- example with counter?

-- EXERCISE
-- Ways to run state actions
-- Below are a few different convenience functions to run state actions.
-- They will be used in examples below

-- EXERCISE
-- Convenient synonym to flip the arguments to runState, so that you can specify the initial state first.
usingState :: s -> State s a -> (s, a)
usingState = undefined

-- EXERCISE
-- Convenient way to run a state action, only returning the final value and discarding the state.
evalUsingState :: s -> State s a -> a
evalUsingState = undefined

-- EXERCISE
-- Convenient way to run a state action, only returning the final state and discarding the value.
execUsingState :: s -> State s a -> s
execUsingState = undefined

-- EXERCISE
-- Below are a few different "primitive" state actions.
-- Once you have get+put+monadic interface you don't really need to (and you should not) use the constructor of State anymore

-- EXERCISE
-- Get the current state
-- EXAMPLES
-- >>> evalUsingState 5 get
-- 5
get :: State s s
get = undefined

-- EXERCISE
-- Replace the current state
-- >>> execUsingState 5 $ put 13
-- 13
put :: s -> State s ()
put = undefined

-- EXERCISE
-- Modify the current state
-- >>> execUsingState 5 $ modify (*13)
-- 65
modify :: (s -> s) -> State s ()
modify = undefined

-- EXERCISE
-- > bracketState before after act
-- Execute before, run act, execute after
-- The idea here is to have a safe pattern to safely locally modify the state for only a single
-- so that you can't forget to call the cleanup function (after).
-- EXAMPLES
-- >>> usingState 5 $ bracketState (modify (+13)) (modify (subtract 13)) get
-- (5,18)
bracketState :: State s () -> State s () -> State s a -> State s a
bracketState = undefined

-- EXERCISE
-- Make State an instance of Functor
-- EXAMPLES for fmap
-- >>> usingState 8 $ fmap (+5) get
-- (8,13)
-- >>> usingState 8 $ fmap (const "ignoring the () that put returns") $ put 13
-- (13,"ignoring the () that put returns")
-- HINT:
-- Use let bindings
instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap = undefined

-- EXERCISE
-- Make State an instance of Applicative
-- EXAMPLES for pure
-- >>> evalUsingState undefined $ pure 'a'
-- 'a'
--
-- EXAMPLES for liftA2
-- >>> evalUsingState 5 $ liftA2 (+) get get
-- 10
-- >>> evalUsingState 5 $ liftA2 (\_ y -> y * 10) (put 10) get
-- 100
-- >>> usingState 5 $ liftA2 (\x _ -> x) get (put 13)
-- (13,5)
instance Applicative (State s) where
  -- We want the action that uses *no* state, and simply returns the given value.
  pure :: a -> State s a
  pure = undefined

  -- We want to sequence these actions left-to-right, i.e. first run the first action, then use its state for the second action,
  -- then combine their results using the provided function.
  -- HINT: use let bindings
  liftA2 :: (a -> b -> c) -> State s a -> State s b -> State s c
  liftA2 = undefined

-- EXERCISE
-- Some very convenient Applicative operators

-- Execute two actions, ignoring the result of the second one.
(<*) :: (Applicative f) => f a -> f b -> f a
(<*) = undefined

infixl 4 <*

-- Execute two actions, ignoring the result of the first one.
-- EXAMPLES
-- >>> usingState 5 $ put 13 *> get
-- (13,13)
-- >>> Nothing *> Just 5
-- Nothing
(*>) :: (Applicative f) => f a -> f b -> f b
(*>) = undefined

infixl 4 *>

-- EXERCISE
-- Use the applicative functions and State to calculate the average for a list.
calcAvg :: [Int] -> Double
calcAvg = undefined
  where
    -- recursive go through the list, modifying the state at each element
    go :: [Int] -> State (Int, Int) ()
    go = undefined

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  -- HINT: use let bindings
  (>>=) = undefined

-- EXERCISE
-- An effectful "map" - notice the similarities with
-- map :: (a -> b) -> [a] -> [b]
-- We're going through the list, "executing" all the actions in order and collecting the results.
-- EXAMPLE
-- >>> traverse Just [1,2,3]
-- Just [1,2,3]
-- >>> traverse (\x -> if even x then Just x else Nothing) [1,2,3]
-- Nothing
-- >>> traverse (\x -> if even x then Just x else Nothing) [2,4,6]
-- Just [2,4,6]
-- >>> execUsingState 0 $ traverse put [1,2,3]
-- 3
-- >>> execUsingState 0 $ traverse (\x -> modify (+x)) [1,2,3]
-- 6
traverse :: (Applicative m) => (a -> m b) -> [a] -> m [b]
traverse = undefined

-- EXERCISE
-- We'll be implementing an interpreter for a small language with two parts - a pure *expression* language and imperative top level statements to execute, top-to-bottom.

-- A variable can be any string. In reality some strings won't occur, such as "".
-- When looking up variables, if they don't exist, use a default value of 0 (woo golang).
type Var = String

-- We'll have only two kinds of top-level statements - printing a variables value, and binding a variable to an expression.
data Statement
  = -- | print the value of the given variable
    Print Var
  | -- | set the given variable to have an expression as a value, e.g. x = 5 + 30
    Bind Var Expr
  deriving (Show, Read)

-- Our expressions are similarly simple.
data Expr
  = -- | numbers
    Val Integer
  | -- | variables
    Var Var
  | -- | let bindings, like in haskell/scheme, i.e. bind the given variable to the given expression for the scope of the other expression
    Let Var Expr Expr
  | -- | add two values, so we have at least some operation to execute
    Add Expr Expr
  deriving (Show, Read)

-- A program is simply a list of statements.
-- You can imagine that these live in some file
type Program = [Statement]

-- We first need to be able to evaluate the pure part of our language.
-- To that end, we introduce an environment which we will keep as part of our state.
-- CONVENTION:
-- We'll always push new variables at the front, and we'll always stop on the first occurence of a variable when looking for it (this is what lookup does)
type Env = [(Var, Expr)]

-- EXERCISE
-- Extend the current environment with the given variable + expr combination
-- EXAMPLES
-- >>> execUsingState [] $ extend "x" (Val 5)
-- [("x",Val 5)]
-- >>> execUsingState [("y", Add (Val 5) (Var "x"))] $ extend "x" (Val 5)
-- [("x",Val 5),("y",Add (Val 5) (Var "x"))]
extendEnv :: Var -> Expr -> State Env ()
extendEnv = undefined

-- EXERCISE
-- Remove the top-most binding from the environment
-- EXAMPLES
-- >>> execUsingState [] popEnv
-- []
-- >>> execUsingState [("x", Val 13)] popEnv
-- []
-- >>> execUsingState [("x", Val 13), ("y", Val 42)] popEnv
-- [("y",Val 42)]
popEnv :: State Env ()
popEnv = undefined

-- EXERCISE
-- Look up the given variable in the current environment.
-- You can use
-- lookup :: String -> [(String, Expr)] -> Maybe Expr
-- for the pure part of this function
-- EXAMPLES
-- >>> evalUsingState [("x",Val 5)] $ lookupEnv "x"
-- Just (Val 5)
-- >>> evalUsingState [] $ lookupEnv "x"
-- Nothing
-- >>> evalUsingState [("x", Val 13), ("x",Val 5)] $ lookupEnv "x"
-- Just (Val 13)
lookupEnv :: Var -> State Env (Maybe Expr)
lookupEnv = undefined

-- EXERCISE
-- Evaluation is then done in the state monad, holding an environment as a state.
-- EXAMPLES
-- >>> evalUsingState [] $ eval (Val 5)
-- 5
-- >>> evalUsingState [] $ eval (Var "x")
-- 0
-- >>> evalUsingState [("x", Val 13)] $ eval (Var "x")
-- 13
-- >>> evalUsingState [("x", Add (Val 5) (Var "y")), ("y", Val 8)] $ eval (Var "x")
-- 13
-- >>> evalUsingState [("x", Add (Val 5) (Var "y")), ("y", Val 8)] $ eval (Add (Var "y") (Var "x"))
-- 21
-- >>> usingState [] $ eval (Let "x" (Val 5) (Var "x"))
-- ([],5)
-- >>> usingState [] $ eval (Let "x" (Let "y" (Val 13) (Var "y")) (Var "x"))
-- ([],13)
eval :: Expr -> State Env Integer
eval = undefined

-- Implementing let by sharing an environment the global mutable one results in some wonky semantics - can you find them?

-- EXERCISE
-- Run a single statement.
-- We return (Maybe String) because we don't always have something to print - in the Bind case, we don't want to print anything.
-- If we don't find the variable we want to Print, we can instead print some warning about it being missing.
-- EXAMPLES
-- >>> evalUsingState [] $ runStatement $ Print "x"
-- Just "x was not found!"
-- >>> evalUsingState [("x", Val 5)] $ runStatement $ Print "x"
-- Just "5"
-- >>> evalUsingState [("x", Add (Val 5) (Var "y")), ("y", Val 8)] $ runStatement $ Print "x"
-- Just "13"
-- >>> usingState [] $ runStatement $ Bind "x" (Val 5)
-- ([("x",Val 5)],Nothing)
-- >>> usingState [("y", Val 10)] $ runStatement $ Bind "x" (Val 5)
-- ([("x",Val 5),("y",Val 10)],Nothing)
-- >>> usingState [("x", Val 10)] $ runStatement $ Bind "x" (Val 5)
-- ([("x",Val 5),("y",Val 10)],Nothing)
--
-- HINTs:
-- You can use
-- lookup :: String -> [(String, a)] -> Maybe a
-- to find things in the environemnt
--
-- Look at what functions you have for operating over `State`
runStatement :: Statement -> State Env (Maybe String)
runStatement = undefined

-- EXERCISE
-- Once we have evaluation and can run individual statements, we can then run our whole program. We will run printing "purely", by building a list of strings to output.
-- We have this function primarily for debugging purposes, as in most cases we won't be interested in our final environment after executing a program.
--
-- HINTS:
-- You'll need to actually run the state action you're building up, as we don't return a value in the State monad here.
-- Think about how to use traverse here
-- EXAMPLES
-- >>> runInState [Print "x"]
-- ([],["x was not found!"])
-- >>> runInState [Bind "x" (Val 5), Print "x"]
-- ([("x",Val 5)],["5"])
-- >>> runInState [Bind "x" (Val 5), Print "x", Bind "y" (Let "z" (Add (Var "x") (Val 5)) (Add (Var "z") (Var "x"))), Print "y"]
-- ([("y",Let "z" (Add (Var "x") (Val 5)) (Add (Var "z") (Var "x"))),("x",Val 5)],["5","15"])
runInState :: Program -> (Env, [String])
runInState = undefined

-- EXERCISE
-- We usually don't need the final environment, so we can implement a convenient synonym.
-- EXAMPLES
-- >>> run [Print "x"]
-- ["x was not found!"]
-- >>> run [Bind "x" (Val 5), Print "x"]
-- ["5"]
-- >>> run [Bind "x" (Val 5), Print "x", Bind "y" (Let "z" (Add (Var "x") (Val 5)) (Add (Var "z") (Var "x"))), Print "y"]
-- ["5","15"]
run :: Program -> [String]
run = undefined

-- EXERCISE
-- Much like fmap lifts a function to work over effectful values, (<*>) allows the function itself to be produced by an effectful value.
-- This function is actually used more often when introducing Applicative and is equivalent to liftA2
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
(<*>) = undefined

-- EXERCISE
-- You can implement an arbitrary liftAn using only liftA2/(<*>) (it's easier with (<*>))
liftA3 :: (Applicative f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 = undefined

-- EXERCISE
-- This function should be equivalent to (>>=), given all the other constraints.
join :: (Monad m) => m (m a) -> m a
join = undefined

-- don't use (>>=) or do-syntax to implement this
bindWithJoin :: (Monad m) => m a -> (a -> m b) -> m b
bindWithJoin = undefined

-- EXERCISE
-- Effectful composition - like (.) but with actions
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = undefined
