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

module IO where

import Text.Read (readMaybe)
import Prelude hiding (getLine, putStrLn, readLn)

-- |
-- building blocks:
-- @do@ and @pure@
-- [x | x <- xs]

-- IO a
-- "действие, което когато го пуснем
-- ще произведе a"

mockString :: IO String
mockString = pure "pesho"

-- data () = ()
someAction :: IO ()
someAction = do
  x <- mockString
  putStrLn x
  getLine :: IO String
  putStrLn x
  putStrLn x
  putStrLn x

-- pure :: a -> IO a

-- World -> (World, a)

-- pure :: a -> World -> (World, a)
-- pure x currWorld = (currWorld, x)

-- data IO a
-- pure :: a -> IO a

-- pure :: a -> Maybe a

-- reminder about()
-- getChar, putChar
--
-- getChar :: IO Char
-- putChar :: Char -> IO ()
--
-- implement putStrLn
-- implement getLine
--
-- @error@
--
-- readFile, writeFile
--
-- type FilePath = String
-- readFile :: FilePath -> IO String
-- writeFile :: FilePath -> String -> IO ()
todo :: todo
todo = todo

get2Char :: IO (Char, Char)
get2Char = do
  (x :: Char) <- (getChar :: IO Char)
  y <- getChar
  -- pure :: a -> IO a
  pure ((x, y) :: (Char, Char)) ::
    IO (Char, Char)

getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n'
    then pure []
    else do
      xs <- getLine
      pure $ x : xs

-- putChar :: Char -> IO ()
putStrLn :: String -> IO ()
putStrLn [] = putChar '\n'
putStrLn (x : xs) = do
  putChar x
  putStrLn xs

-- error :: String -> a

-- writeFile :: String -> String -> IO ()
-- readFile :: String -> IO String

-- Use @error :: String -> a@ when you need to error out.

-- | EXERCISE
-- Implement reading an @Integer@ from stdin.
-- Use @readMaybe :: Read a => String -> Maybe a@
getNumber :: IO Integer
getNumber = do
  str <- getLine
  case readMaybe str of
    Nothing -> error "not a number"
    Just n -> pure n

-- | EXERCISE
-- Implement reading an @Bool@ from stdin.
getBool :: IO Bool
getBool = do
  str <- getLine
  case readMaybe str of
    Nothing -> error "not a bool"
    Just n -> pure n

-- | EXERCISE
-- Implement a function which runs the provided action when
-- the given @Maybe@ is a nothing, and otherwise returns the value within the @Just@.
-- This is actually a common bit in both @getBool@ and @getNumber@
-- and is an overall very useful function.
whenNothing :: Maybe a -> IO a -> IO a
whenNothing Nothing mx = mx
whenNothing (Just x) _ = pure x

-- | EXERCISE
-- @getNumber@ and @getBool@ are practically the same.
-- We can generalise them to use the @Read@ type class, so that they
-- work for any type which has an instance of @Read@.
-- Try to use @whenNothing@ here.
--
-- Note that sometimes when using this function you'll need to specify a type annotation,
-- because the compiler will not be able to figure out what exactly type you want to read.
-- e.g. @x <- readLn :: IO Int@ to read an @Int@, or similarly you could do @x :: Int <- readLn@
readLn :: (Read a) => IO a
readLn = do
  res <- readLnMay
  whenNothing res $ error "no parse"

readLnMay :: (Read a) => IO (Maybe a)
readLnMay = do
  str <- getLine
  pure $ readMaybe str

-- | EXERCISE
-- Run an IO action only when the given @Bool@ is @True 2
when :: Bool -> IO () -> IO ()
when False _ = pure ()
when True mx = mx

-- | EXERCISE
-- Repeat a @Maybe a@ producing action, until it produces a @Just@, returning the result.
untilJustM :: IO (Maybe a) -> IO a
untilJustM mx = do
  xMay <- mx
  case xMay of
    Nothing -> untilJustM mx
    Just x -> pure x

-- | EXERCISE
-- We're going to implement a very simplified version of the Hangman game.
--
-- The @startHangman@ function takes in an argument a path to a file containing a dictionary of words.
-- The dictionary is expected to be newline seperated for each word, and to have at least 100 words.
-- You can find such a dictionary in directory of this file called @words.txt@
--
-- After doing that, it prompts the user for a number between 0 and 99, inclusive, and then
-- uses that number as an index into the dictionary to pick a word for the game hangman.
-- If the user errors a number which is outside that range, we should prompt them again (@untilJustM@ is useful here)
--
--
-- The @playHangman guessedSoFar target@ function is the actual "gameplay" we're going to implement.
-- In it, we keep an argument @guessedSoFar@ as "state" for the letters we've guessed so far,
-- as well as which the target word is.
-- On each turn of the game, we must
-- 1. Print out the "guessed" version of the target word, i.e., we display only letters which the player has
--    guessed so far, "censoring" the others with some symbol (e.g. '-' or '_')
-- 1. Ask the player to make a guess for what the word is
-- 2. If the player guesses the word, we terminate the game and print a cheerful message
-- 3. Otherwise, we continue playing the game, extending the list of guessed letters.
startHangman :: String -> IO ()
startHangman filePath = do
  contents <- readFile filePath
  let wordsInDict = lines contents
  when (length wordsInDict < 100) $ error "expected at least 100 lines in the dictionary"
  wordIndex <- getAppropriateNumber
  playHangman [] $ wordsInDict !! wordIndex
  where
    getAppropriateNumber :: IO Int
    getAppropriateNumber = untilJustM $ do
      putStrLn "Please enter a number between 0 and 99"
      numMay <- readLnMay
      pure $
        case numMay of
          Just n | n >= 0 && n <= 99 -> Just n
          _ -> Nothing

playHangman :: [Char] -> String -> IO ()
playHangman guessed targetWord = do
  putStrLn $ "Current guess is: " ++ censor targetWord
  putStrLn "Please try to guess the word."
  guess <- getLine
  if guess == targetWord || all isGuessed targetWord
    then putStrLn "you win!!!!!"
    else playHangman (guess ++ guessed) targetWord
  where
    censor :: String -> String
    censor = map (\x -> if isGuessed x then x else '_')
    isGuessed c = c `elem` guessed

-- | EXERCISE
-- Run an IO action an infinite amount of times
forever :: IO a -> IO b
forever mx = do
  mx
  forever mx

-- | EXERCISE
-- Map a function over the result of an IO action.
mapIO :: (a -> b) -> IO a -> IO b
mapIO f mx = do
  x <- mx
  pure $ f x

-- | EXERCISE
-- "Lift" a function of two arguments to work over two IO actions instead.
lift2IO :: (a -> b -> c) -> IO a -> IO b -> IO c
lift2IO f mx my = do
  x <- mx
  y <- my
  pure $ f x y

-- | EXERCISE
-- Read two numbers and sum them using using @lift2IO@
sumTwo :: IO Integer
sumTwo = lift2IO (+) getNumber getNumber

-- | EXERCISE
-- Given an IO action producing a function, and an IO action producing an argument for that function
-- run thefunction over the argument.
-- Try to implement this using @lift2IO@
apIO :: IO (a -> b) -> IO a -> IO b
apIO mf mx = lift2IO ($) mf mx

-- | EXERCISE
-- Lift a three argument function to work over three IO actions.
-- Try to use @apIO@, @pure@/@mapIO@ to implement this
lift3IO :: (a -> b -> c -> d) -> IO a -> IO b -> IO c -> IO d
lift3IO f mx my mz = pure f `apIO` mx `apIO` my `apIO` mz

-- | EXERCISE
-- Given a number and an IO action, run that IO action the number many times,
-- returning all the results in a list.
-- Try to also implement this with @lift2IO@.
replicateIO :: Int -> IO a -> IO [a]
replicateIO 0 _ = pure []
replicateIO n mx = lift2IO (:) mx $ replicateIO (n - 1) mx

-- | EXERCISE
-- For each number in a list, read in that many strings from stdin, returning them in a list of lists.
readInLists :: [Int] -> IO [[String]]
readInLists [] = pure []
readInLists (n : ns) = do
  strs <- replicateIO n getLine
  strss <- readInLists ns
  pure $ strs : strss

-- | EXERCISE
-- Map over a list, executing an IO action for each element, and collect the results in a list.
traverseListIO :: (a -> IO b) -> [a] -> IO [b]
traverseListIO _f [] = pure []
traverseListIO f (x : xs) = lift2IO (:) (f x) $ traverseListIO f xs

-- | EXERCISE
-- Implement @readInLists@ using @traverseListIO@
readInLists' :: [Int] -> IO [[String]]
readInLists' = traverseListIO (`replicateIO` getLine)

-- EXERCISE:
-- Extend the hangman game to support a turn limit.
-- If the player wants to win, they must guess the word before the turn limit.
