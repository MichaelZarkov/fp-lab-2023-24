{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
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

module ADTs where

-- | TODO: talk about
-- * pls write on discord+join discord
-- * solutions (tagged on discord)
-- * first homework this week
-- show:
-- * pragmas on the top of files
-- * where from last week
-- * guards
-- * remind about sections
todo :: todo
todo = todo

-- | RPS - enum example
-- show case here
-- deriving Show
data RPS
  deriving (Show)

-- mention Bool

-- |
-- show ignore pattern match
-- pattern evaluation order
beats :: RPS -> RPS -> Bool
beats = undefined

-- |
-- analogue with C structs or something
-- record syntax
-- example point
-- mention constructor name requirements, Mk convention
-- tuples
data Point
  deriving (Show)

isInFirstQuadrant :: Point -> Bool
isInFirstQuadrant = undefined

invert :: Point -> Point
invert = undefined

-- | cats n dogs
-- colours n breeds
-- example for animal value
-- example with animal matching
-- showAnimal
data Animal
  deriving (Show)

-- | explain the encoding (peano)
data Nat
  deriving (Show)

-- implement
integerToNat :: Integer -> Nat
integerToNat = undefined

-- | implement
-- evaluate manually?
natToInteger :: Nat -> Integer
natToInteger = undefined

-- | implement
-- evaluate manually?
addNat :: Nat -> Nat -> Nat
addNat = undefined

-- TASK
-- define what the "next" throw you can do is in the "usual" ordering of RPS
-- i.e. @next x@ should be the throw that beats x
next :: RPS -> RPS
next = undefined

-- TASK
-- define what it means for two RPS values to be equal
-- in general for a type, this would mean that the constructors must be equal
-- and all their contents should all so be equal (pointwise)
-- for an "enum" in particular, this only leaves the constructor check
-- use _ matches!
-- EXAMPLES
-- >>> eqRPS Rock Rock
-- True
-- >>> eqRPS Rock Paper
-- False
eqRPS :: RPS -> RPS -> Bool
eqRPS = undefined

-- TASK
-- define a shorter version of beats by uisng next and eqRPS
-- EXAMPLES
-- >>> beats' Rock Paper
-- True
-- >>> beats' Rock Scissors
-- False
-- >>> beats' Paper Scissors
-- True
beats' :: RPS -> RPS -> Bool
beats' = undefined

-- TASK
-- Your task here is related to modelling the game of Belote, i.e. you will be
-- implementing different data types which represent game concepts from the game,
-- as well as some functions to calculate a few parts of the game.

-- | Implement a data type for Ranks (7 8 9 10 J etc)
data Rank
  deriving (Show)

-- | Implement a data type for Suits
data Suit
  deriving (Show)

-- | Check if two Suits are the same
suitEquals :: Suit -> Suit -> Bool
suitEquals = undefined

-- | Implement a data type for a Card
-- Use record syntax.
data Card = MkCard
  deriving (Show)

-- | Implement a data type for Contracts (all trump, no trump etc)
data Contract
  deriving (Show)

-- | Given a Card and a Contract, implement a check whether the card is of a trump suit
isTrump :: Contract -> Card -> Bool
isTrump = undefined

-- | Given a card and a contract, assign a numerical "power value" to it
-- The power value should be such that the following property holds:
-- ∀contract ∀card1 ∀card2. "@card1@ beats @card2@ in @contract@" => @cardPower contract card1 > cardPower contract card2@
cardPower :: Contract -> Card -> Integer
cardPower = undefined

-- | A data type to describe the different ways two cards can relate, given a contract.
-- See the 'sameSuit' and 'relateCards' functions below to get a better sense of how
-- you'll be producing this data type, and hence what constructors it should have.
--
-- This data type exists mainly because it's useful as a tool to implement the 'fight' function.
-- As such, it might be the case that your version of CardRelation is different from what I intended.
--
-- The way to think about this is as follows:
-- Imagine you're in a situation where someone has played a card, and you've just played a card.
-- You now need to decide which card would beat the other one.
-- 'CardRelation' expresses the first thing you need to calculate in regards to the two cards
-- *before* you can start checking their 'cardPower's, e.g. is one of them a trump and so on.
--
-- HINT:
-- My intended solution has 4 constructors.
data CardRelation

-- | Calculate whether two cards have the same suit.
-- The 'CardRelation' data type should support expressing "these two cards have the same suit" or
-- "these two cards have different suits".
sameSuit :: Card -> Card -> CardRelation
sameSuit = undefined

-- | Given a contract, calculate the relation between two cards.
-- Since this is the main use of the 'CardRelation' data type, you should be using all of the
-- constructors of 'CardRelation' here.
relateCards :: Contract -> Card -> Card -> CardRelation
relateCards = undefined

-- | Return the "stronger" card according to @cardPower@
maximumCard :: Contract -> Card -> Card -> Card
maximumCard = undefined

-- | Given two Cards and a Contract, return the Card that would "win" (according to their power)
-- when playing under the given Contract
-- Assume that @card1@ is played first.
fight :: Contract -> Card -> Card -> Card
fight = undefined

-- | A Trick consisting of four cards
-- Again, assume that "the earlier" a card is in the Trick constructor, the earlier it was played in that trick.
data Trick
  deriving (Show)

-- | Given a Contract and a Trick of four cards, calculate which card would be the winner of the Trick
-- Don't forget the "left to right" rule of Trick, wherein cards "further left" in the constructor's arguments
-- are played earlier in the trick
winner :: Contract -> Trick -> Card
winner = undefined

-- TASK
-- multiply two @Nat@s recursively, much like we did with Ints last time
-- EXAMPLES
-- >>> multNat Zero (Succ (Succ (Succ Zero)))
-- Zero
-- >>> multNat (integerToNat 2) (integerToNat 3)
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
multNat :: Nat -> Nat -> Nat
multNat = undefined

-- TASK
-- 'Ordering' is a datatype which is built into the standard library
-- that is made to mean "the result of a comparison" or "the ordering between two things".
-- It's defined like so:
-- @data Ordering = LT | EQ | GT@
-- with the constructors being L(ess)T(han), EQ(ual) G(reater)T(han)
-- implement a comparison for @Nat@s, returning an @Ordering@
-- EXAMPLES
-- >>> compareNat (Succ Zero) (Succ Zero)
-- EQ
-- >>> compareNat Zero (Succ Zero)
-- LT
-- >>> compareNat (Succ Zero) Zero
-- GT
compareNat :: Nat -> Nat -> Ordering
compareNat = undefined

-- TASK
-- calculate the larger of two @Nat@s recursively
-- EXAMPLES
-- >>> maxNat (Succ Zero) Zero
-- Succ Zero
-- >>> maxNat (Succ (Succ Zero)) Zero
-- Succ (Succ Zero)
-- >>> maxNat (Succ (Succ Zero)) (Succ (Succ (Succ (Succ Zero))))
-- Succ (Succ (Succ (Succ Zero)))
maxNat :: Nat -> Nat -> Nat
maxNat = undefined

-- | README
-- the "syntax" for a very basic "calculator" datatype
-- or alternatively, a very simple programming language
--
-- we can build up Expr(essions) by
-- * injecting integers as a value directly - Val
-- * stating that we want to add the result of two calculations - Plus
-- * stating that we want to multiply the result of two calculations - Mult
data Expr
  = Val Integer
  | Plus Expr Expr
  | Mult Expr Expr
  deriving (Show)

-- README - SECTIONS
--
-- Writing
-- Plus (Val 3) (Plus (Val 4) (Val 5))
-- is annoying - a lot of parens
-- we can abuse sections to write "prettier" expressions
-- Val 3 `Plus` Val 4
-- is the same as
-- Plus (Val 3) (Val 4)
-- But what would
-- Val 3 `Plus` Val 4 `Plus` Val 5
-- be?
-- We can use these pragmas
infixr 7 `Plus`

infixr 8 `Mult`

-- infixr(ight)
-- to tell the compiler that when used in a section/as operators
-- Mult has higher priority than Plus, e.g.
-- Val 3 `Plus` Val 4 `Mult` Val 5
-- means
-- Val 3 `Plus` (Val 4 `Mult` Val 5)
-- and
-- Plus and Mult are both right associative, e.g.
-- Val 3 `Plus` Val 4 `Plus` Val 5
-- means
-- Val 3 `Plus` (Val 4 `Plus` Val 5)

-- TASK
-- and now that we have the syntactic structure of the computation we want to make
-- we can implement its semantics by writing an evaluator for our calculator
-- or alternatively an interpreter for our programming language
-- EXAMPLES
-- >>> eval (Val 3)
-- 3
-- >>> eval (Plus (Val 3) (Val 4))
-- 7
-- >>> eval (Val 33 `Mult` Val 36)
-- 1188
-- >>> eval ((Val 3 `Plus` Val 3) `Mult` Val 7)
-- 42
-- >>> eval (Val 3 `Plus` Val 3 `Mult` Val 7)
-- 24
eval :: Expr -> Integer
eval = undefined

-- TASK
-- add an If expression to our Expr language
-- by using other calculations for our "condition value"
-- extend eval so that it also works with the new If construction
-- interpreting 0 as "false" and any other value as "true"

-- TASK
-- add a name to the Animal type
--
-- is there more than one way to add it to Animal?
-- which way would be more convenient for the implementation of the following function?
-- introduce :: Animal -> String
-- which shows the name of the animal

-- TASK
-- Check if a Trick could have possibly been played according to the games of Belote
isValid :: Contract -> Trick -> Bool
isValid = undefined
