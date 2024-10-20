module Lib1
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition
    ) where

import Data.Char (isDigit)

data Query = Concat String String
           | FMotif String String
           | Complement String
           | Transcribe String
           | Mutate String Integer
           deriving (Show, Eq)

charParser :: Char -> String -> Either String (Char, String)
charParser c [] = Left ("Cannot find " ++ [c] ++ " in an empty list")
charParser c (h:t)
  | c == h    = Right (c, t)
  | otherwise = Left ([c] ++ " is not the first element of " ++ t)

stringParser :: String -> String -> Either String String
stringParser [] input = Right input
stringParser (h:t) input =
  case charParser h input of
    Left err -> Left err
    Right (_, rest) -> stringParser t rest

-- <integer> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | <integer> <integer>
parseInt :: String -> Either String Integer
parseInt [] = Left "Error: no digits found."
parseInt input = parseDigits 0 input

-- | Helper for `parseInt`.
parseDigits :: Integer -> String -> Either String Integer
parseDigits acc [] = Right acc
parseDigits acc (h:t)
  | isDigit h = parseDigits (acc * 10 + toInteger (fromEnum h - fromEnum '0')) t
  | otherwise = Left ("Error: '" ++ [h] ++ "' is not a valid digit.")

-- <nucleotide> ::= "A" | "T" | "C" | "G"
parseNucleotide :: Char -> Either String Char
parseNucleotide 'A' = Right 'A'
parseNucleotide 'T' = Right 'T'
parseNucleotide 'U' = Right 'U'
parseNucleotide 'C' = Right 'C'
parseNucleotide 'G' = Right 'G'
parseNucleotide _   = Left "Error: invalid nucleotide."

-- | Parses a nucleotide sequence (string of valid nucleotides).
-- <sequence> ::= <nucleotide> | <nucleotide> <sequence>
parseNucSeq :: String -> Either String (String, String)
parseNucSeq [] = Right ([], [])
parseNucSeq (h:t)
  | h == ' '  = Right ([], h:t)
  | otherwise = case parseNucleotide h of
      Left err -> Left err
      Right nucleotide -> case parseNucSeq t of
          Left err -> Left err
          Right (nucSeq, remainder) -> Right (nucleotide : nucSeq, remainder)

-- <operation> ::= "concat" <operand> <operand>
parseConcat :: String -> Either String Query
parseConcat input =
  case stringParser "concat " input of
    Left err -> Left err
    Right rest1 -> case parseNucSeq rest1 of
      Left err -> Left err
      Right (seq1, rest2) -> case stringParser " " rest2 of
        Left err -> Left err
        Right rest3 -> case parseNucSeq rest3 of
          Left err -> Left err
          Right (seq2, _) -> Right (Concat seq1 seq2)

-- <operation> ::= "find-motif" <operand> <operand>
parseFMotif :: String -> Either String Query
parseFMotif input =
  case stringParser "fmotif " input of
    Left err -> Left err
    Right rest1 -> case parseNucSeq rest1 of
      Left err -> Left err
      Right (seq1, rest2) -> case stringParser " " rest2 of
        Left err -> Left err
        Right rest3 -> case parseNucSeq rest3 of
          Left err -> Left err
          Right (seq2, _) -> Right (FMotif seq1 seq2)

-- <operation> ::= "complement" <operand>
parseComplement :: String -> Either String Query
parseComplement input =
  case stringParser "complement " input of
    Left err -> Left err
    Right rest -> case parseNucSeq rest of
      Left err -> Left err
      Right (seq1, _) -> Right (Complement seq1)

-- <operation> ::= "transcribe" <operand>
parseTranscribe :: String -> Either String Query
parseTranscribe input =
  case stringParser "transcribe " input of
    Left err -> Left err
    Right rest -> case parseNucSeq rest of
      Left err -> Left err
      Right (seq1, _) -> Right (Transcribe seq1)

-- <operation> ::= "mutate" <operand> <percentage>
parseMutate :: String -> Either String Query
parseMutate input =
  case stringParser "mutate " input of
    Left err -> Left err
    Right rest1 -> case parseNucSeq rest1 of
      Left err -> Left err
      Right (seq1, rest2) -> case stringParser " " rest2 of
        Left err -> Left err
        Right rest3 -> case parseInt rest3 of
          Left err -> Left err
          Right int -> Right (Mutate seq1 int)

-- <operation> ::= "concat" <operand> <operand>
--               | "find-motif" <operand> <operand>
--               | "complement" <operand>
--               | "transcribe" <operand>
--               | "mutate" <operand> <percentage>
parseQuery :: String -> Either String Query
parseQuery input
  | take 7 input == "concat "    = parseConcat input
  | take 7 input == "fmotif "    = parseFMotif input
  | take 11 input == "complement " = parseComplement input
  | take 11 input == "transcribe " = parseTranscribe input
  | take 7 input == "mutate "    = parseMutate input
  | otherwise = Left "Error: unknown command."

-- | Represents the program state, holding the current nucleotide sequence.
data State = State {
  nucleotideSequence :: String
} deriving (Show, Eq)

-- | Initial state of the program.
emptyState :: State
emptyState = State { nucleotideSequence = "" }

-- | Updates the state based on a query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition currentState (Concat seq1 seq2) =
  let newSequence = seq1 ++ seq2
  in Right (Just ("Concatenated: " ++ newSequence), currentState { nucleotideSequence = newSequence })

stateTransition currentState (FMotif seq1 seq2) =
  if containsMotif seq1 seq2
  then Right (Just ("Motif " ++ seq2 ++ " found in " ++ seq1), currentState)
  else Left "Error: Motif not found."

stateTransition currentState (Complement seq1) =
  case complementSequence seq1 of
    Left err -> Left err
    Right compSeq -> Right (Just ("Complement: " ++ compSeq), currentState { nucleotideSequence = compSeq })

stateTransition currentState (Transcribe seq1) =
  let transcribedSeq = transcribeSequence seq1
  in Right (Just ("Transcribed: " ++ transcribedSeq), currentState { nucleotideSequence = transcribedSeq })

stateTransition currentState (Mutate seq1 percentage) =
  let mutatedSeq = mutate seq1 percentage
  in Right (Just ("Mutated sequence: " ++ mutatedSeq), currentState { nucleotideSequence = mutatedSeq })

containsMotif :: String -> String -> Bool
containsMotif [] _ = False
containsMotif seq1 seq2 =
  if startsWith seq1 seq2
  then True
  else containsMotif (tail seq1) seq2

startsWith :: String -> String -> Bool
startsWith [] _ = False
startsWith _ [] = True
startsWith (x:xs) (y:ys) = x == y && startsWith xs ys

complementSequence :: String -> Either String String
complementSequence [] = Right []
complementSequence (h:t) =
  case complementNucleotide h of
    Left err -> Left err
    Right comp -> case complementSequence t of
      Left err -> Left err
      Right rest -> Right (comp : rest)

complementNucleotide :: Char -> Either String Char
complementNucleotide 'A' = Right 'T'
complementNucleotide 'T' = Right 'A'
complementNucleotide 'C' = Right 'G'
complementNucleotide 'G' = Right 'C'
complementNucleotide _ = Left "Error: Invalid nucleotide for complement."

-- Transcribes a DNA sequence (replacing 'T' with 'U' for RNA).
transcribeSequence :: String -> String
transcribeSequence = map transcribeNucleotide

transcribeNucleotide :: Char -> Char
transcribeNucleotide 'T' = 'U'
transcribeNucleotide nucleotide = nucleotide

mutate :: String -> Integer -> String
mutate seq percentage =
  let numToMutate = fromIntegral (length seq) * percentage `div` 100
      (toMutate, rest) = splitAt (fromInteger numToMutate) seq
  in map mutateNucleotide toMutate ++ rest

-- | Mutates a single nucleotide to another random nucleotide (simplified).
mutateNucleotide :: Char -> Char
mutateNucleotide 'A' = 'T'
mutateNucleotide 'T' = 'A'
mutateNucleotide 'C' = 'G'
mutateNucleotide 'G' = 'C'
mutateNucleotide nucleotide = nucleotide
