{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query where
  Concat :: String -> String -> Query
  FMotif :: String -> String -> Query
  Complement :: String -> Query
  Transcribe :: String -> Query
  Mutate :: String -> Integer -> Query

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input =
  case words input of
    ("concat":x:y:[]) ->
      if isNucSeq x && isNucSeq y
        then Right (Concat x y)
        else Left "Invalid concat arguments"

    ("fmotif":x:y:[]) ->
      if isNucSeq x && isNucSeq y
        then Right (FMotif x y)
        else Left "Invalid fmotif arguments"

    ("complement":x:[]) ->
      if isNucSeq x
        then Right (Complement x)
        else Left "Invalid complement arguments"

    ("transcribe":x:[]) ->
      if isNucSeq x
        then Right (Complement x)
        else Left "Invalid transcribe arguments"

    ("mutate":x:y:[]) ->
      if isNucSeq x && isInt y
        then Right (Complement x)
        else Left "Invalid transcribe arguments"

    _ -> Left "Invalid input"

--TODO
isNucSeq :: String -> Bool
isNucSeq _ = True
--TODO
isInt :: String -> Bool
isInt _ = True


parseNucleotide :: Char -> Either String Char
parseNucleotide 'A' = Right 'A'
parseNucleotide 'T' = Right 'T'
parseNucleotide 'U' = Right 'U'
parseNucleotide 'C' = Right 'C'
parseNucleotide 'G' = Right 'G'
parseNucleotide _ = Left "Error: invalid nucleotide."

parseNucSeq :: String -> Either String String
parseNucSeq [] = Right []
parseNucSeq (x:xs)

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"
