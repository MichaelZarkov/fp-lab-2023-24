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

data RPS
  = Rock
  | Paper
  | Scissors
  deriving (Show)

data Nat
  = Zero
  | Succ Nat
  deriving (Show)

-- >>> integerToNat 5
-- Succ (Succ (Succ (Succ (Succ Zero))))
integerToNat :: Integer -> Nat
integerToNat n =
  if n == 0
    then Zero
    else Succ (integerToNat (n - 1))

-- >>> natToInteger three
-- 3
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

addNat :: Nat -> Nat -> Nat
addNat Zero m = m
addNat (Succ n) m = Succ (addNat n m)

-- TASK
-- define what the "next" throw you can do is in the "usual" ordering of RPS
-- i.e. @next x@ should be the throw that beats x
next :: RPS -> RPS
next Rock = Paper
next Paper = Scissors
next Scissors = Rock

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
eqRPS Rock Rock = True
eqRPS Paper Paper = True
eqRPS Scissors Scissors = True
eqRPS _ _ = False

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
beats' x y = next x `eqRPS` y

-- TASK
-- Your task here is related to modelling the game of Belote, i.e. you will be
-- implementing different data types which represent game concepts from the game,
-- as well as some functions to calculate a few parts of the game.

-- | Implement a data type for Ranks (7 8 9 10 J etc)
data Rank = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show)

-- | Implement a data type for Suits
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show)

-- | Check if two Suits are the same
suitEquals :: Suit -> Suit -> Bool
suitEquals Clubs Clubs = True
suitEquals Diamonds Diamonds = True
suitEquals Hearts Hearts = True
suitEquals Spades Spades = True
suitEquals _ _ = False

-- | Implement a data type for a Card
-- Use record syntax.
data Card = MkCard Rank Suit
  deriving (Show)

-- | Implement a data type for Contracts (all trump, no trump etc)
data Contract = Trumps Suit | NoTrump | AllTrump
  deriving (Show)

-- | Implement a type representing whether a card's "power" in a particular contract
-- i.e. we should be able to represent that a card is a trump, or not a trump
data IsTrump = NotTrump | Trump
  deriving (Show)

-- | Given a Card and a Contract, implement a check whether the card is of a trump suit
isTrump :: Contract -> Card -> Bool
isTrump (Trumps trumpSuit) (MkCard _rank cardSuit) = trumpSuit `suitEquals` cardSuit
isTrump AllTrump _ = True
isTrump NoTrump _ = False

-- | Given a card and a contract, assign a numerical "power value" to it
-- The power value should be such that the following property holds:
-- ∀contract ∀card1 ∀card2. "@card1@ beats @card2@ in @contract@" => @cardPower contract card1 > cardPower contract card2@
cardPower :: Contract -> Card -> Integer
cardPower contract card@(MkCard rank _suit) =
  if isTrump contract card
    then rankPowerTrump rank
    else rankPowerNoTrump rank

rankPowerTrump :: Rank -> Integer
rankPowerTrump rank =
  case rank of
    Seven -> 0
    Eight -> 1
    Queen -> 2
    King -> 3
    Ten -> 4
    Ace -> 5
    Nine -> 6
    Jack -> 7

rankPowerNoTrump :: Rank -> Integer
rankPowerNoTrump rank =
  case rank of
    Seven -> 0
    Eight -> 1
    Nine -> 2
    Queen -> 3
    Jack -> 4
    King -> 5
    Ten -> 6
    Ace -> 7

-- | A data type to describe the different ways two cards can relate, given a contract.
-- See the 'sameSuit' and 'relateCards' functions below to get a better sense of how
-- you'll be producing this data type, and hence what constructors it should have.
data CardRelation
  = FirstIsTrump
  | SecondIsTrump
  | SameSuit
  | DifferentSuit

-- | Given a contract, calculate the relation between two cards.
relateCards :: Contract -> Card -> Card -> CardRelation
relateCards contract card1@(MkCard _ card1Suit) card2@(MkCard _ card2Suit) =
  case (isTrump contract card1, isTrump contract card2) of
    (False, False) ->
      if card1Suit `suitEquals` card2Suit
        then SameSuit
        else DifferentSuit
    (True, True) -> SameSuit
    (True, False) -> FirstIsTrump
    (False, True) -> SecondIsTrump

-- | Return the "stronger" card according to @cardPower@
maximumCard :: Contract -> Card -> Card -> Card
maximumCard contract x y =
  if cardPower contract x <= cardPower contract y
    then y
    else x

-- | Given two Cards and a Contract, return the Card that would "win" (according to their power)
-- when playing under the given Contract
-- Assume that @card1@ is played first.
fight :: Contract -> Card -> Card -> Card
fight contract card1 card2 =
  case relateCards contract card1 card2 of
    DifferentSuit -> card1
    SameSuit -> maximumCard contract card1 card2
    FirstIsTrump -> card1
    SecondIsTrump -> card2

-- | A Trick consisting of four cards
-- Again, assume that "the earlier" a card is in the Trick constructor, the earlier it was played in that trick.
data Trick = MkTrick Card Card Card Card
  deriving (Show)

-- | Given a Contract and a Trick of four cards, calculate which card would be the winner of the Trick
-- Don't forget the "left to right" rule of Trick, wherein cards "further left" in the constructor's arguments
-- are played earlier in the trick
winner :: Contract -> Trick -> Card
winner contract (MkTrick card1 card2 card3 card4) =
  ((card1 `fight'` card2) `fight'` card3) `fight'` card4
  where
    fight' card = fight contract card

-- TASK
-- multiply two @Nat@s recursively, much like we did with Ints last time
-- EXAMPLES
-- >>> multNat Zero (Succ (Succ (Succ Zero)))
-- Zero
-- >>> multNat (integerToNat 2) (integerToNat 3)
-- Succ (Succ (Succ (Succ (Succ (Succ Zero)))))
multNat :: Nat -> Nat -> Nat
multNat Zero _ = Zero
multNat (Succ n) m = addNat m (multNat n m)

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
maxNat Zero m = m
maxNat n Zero = n
maxNat (Succ n) (Succ m) = Succ (maxNat n m)

-- TASK
-- Ordering is a datatype that is made to mean "the result of a comparison" or "the ordering between two things"
-- it's defined like so:
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
compareNat Zero Zero = EQ
compareNat Zero (Succ _) = LT
compareNat (Succ _) Zero = GT
compareNat (Succ n) (Succ m) = compareNat n m

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
  | If Expr Expr Expr
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
eval (Val n) = n
eval (Plus e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2
eval (If cond thn els) =
  if eval cond == 0
    then eval els
    else eval thn

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
--
-- We start with this:
-- data Animal
--   = Cat Colour Double
--   | Dog Breed Integer
--   deriving (Show)
--
-- now we can directly add it to each constructor, getting:
-- data Animal
--   = Cat String Colour Double
--   | Dog String Breed Integer
--
-- This is a bit redundant however, since now we have duplicated Strings in each constructor
-- Additionally, implementing
-- introduce :: Animal -> String
-- introduce (Cat name _ _) = "Hi, my name is " ++ name
-- introduce (Dog name _ _) = "Hi, my name is " ++ name
--
-- We see that the result of the duplicated String in the two constructors means that
-- we also duplicate code when writing functions that need this name.
-- In order to avoid this, one approach would be to rename our current Animal to something
-- indicating that this data type holds information which is only specific to each animal, e.g.
data AnimalType
  = Cat Colour Double
  | Dog Breed Integer

data Breed = Husky | Lab
  deriving (Show)

data Colour = Orange | Black
  deriving (Show)

-- We can now "recreate" Animal, adding the String to be common for all animal types:
data Animal = MkAnimal String AnimalType

-- And we can avoid code duplication in @introduce@:
introduce :: Animal -> String
introduce (MkAnimal name _) = "Hi, my name is " ++ name

-- TASK
-- Check if a Trick could have possibly been played according to the games of Belote
--
-- This one is left as an exercise for the reader.
isValid :: Contract -> Trick -> Bool
isValid = undefined
