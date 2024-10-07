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


  -- >>> charParser 'c' "abc"
-- Left "c is not the first element of bc"
charParser :: Char -> String -> Either String (Char,String)
charParser c [] =
  Left ("Cannot find " ++ [c] ++ " in an empty list")
charParser c (h:t) =
  if c == h then Right(c, t)
  else Left ([c] ++ " is not the first element of " ++ t)

-- >>> stringParser "concat" "concat AAUG TCCG"
-- Right " AAUG TCCG"
stringParser :: String -> String -> Either String String
stringParser [] input = Right input
stringParser (h:t) input =
  case charParser h input of
    Left errorFromChar -> Left errorFromChar
    Right (_, rest) -> stringParser t rest


parseInt :: String -> Either String (Integer, String)
parseInt [] = Left "Error: no digits found."
parseInt input = parseDigits 0 input

parseDigits :: Integer -> String -> Either String (Integer, String)
parseDigits acc [] = Right (acc, [])
parseDigits acc (h:t) 
  | h == '0' = parseDigits (acc * 10 + 0) t
  | h == '1' = parseDigits (acc * 10 + 1) t
  | h == '2' = parseDigits (acc * 10 + 2) t
  | h == '3' = parseDigits (acc * 10 + 3) t
  | h == '4' = parseDigits (acc * 10 + 4) t
  | h == '5' = parseDigits (acc * 10 + 5) t
  | h == '6' = parseDigits (acc * 10 + 6) t
  | h == '7' = parseDigits (acc * 10 + 7) t
  | h == '8' = parseDigits (acc * 10 + 8) t
  | h == '9' = parseDigits (acc * 10 + 9) t
  | otherwise = Right (acc, h:t) -- Stop when a non-digit character is found

  
-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Not implemented 2"

-- >>> parseNucleotide 'u' 
-- Left "Error: invalid nucleotide."
parseNucleotide :: Char -> Either String Char
parseNucleotide 'A' = Right 'A'
parseNucleotide 'T' = Right 'T'
parseNucleotide 'U' = Right 'U'
parseNucleotide 'C' = Right 'C'
parseNucleotide 'G' = Right 'G'
parseNucleotide _ = Left "Error: invalid nucleotide."

-- >>> parseNucSeq "3 GA"
-- Left "Error: invalid nucleotide."
parseNucSeq :: String -> Either String (String, String)
parseNucSeq [] = Right ([], [])
parseNucSeq (h:t) =
  if h == ' ' then --if there was at least one nucleotide and there is a space, stop.
    Right ([], h:t)
  else
    case parseNucleotide h of --if nucleotide h
      Right nucleotide -> --nucleotide h correct
        case parseNucSeq t of --if sequence t (original sequence but without first element)
          Right (nucleotideSeq, remainder) -> Right (nucleotide : nucleotideSeq, remainder) -- =true
          Left parseError -> Left parseError
      Left parseError -> Left parseError



-- >>> parseConcat "concat CCGT g"
-- Left "Error: invalid nucleotide."
parseConcat :: String -> Either String Query
parseConcat input =
  case stringParser "concat " input of --query starts with concat and space?
    Left e1 -> Left e1
    Right rest1 ->
      case parseNucSeq rest1 of --query has a nucleotide sequence?
      Left e2 -> Left e2
      Right (seq1, rest2) -> 
        case stringParser " " rest2 of --query has space after nucleotide sequence?
        Left e3 -> Left e3
        Right rest3 ->
          case parseNucSeq rest3 of --query has a nucleotide sequence after space?
          Left e4 -> Left e4
          Right (seq2, _) -> 
            if null seq2
            then Left "Error: second nucleotide sequence missing."
            else Right (Concat seq1 seq2)

parseFMotif :: String -> Either String Query
parseFMotif input =
  case stringParser "fmotif " input of -- query starts with fmotif and a space?
    Left e1 -> Left e1
    Right rest1 ->
      case parseNucSeq rest1 of -- first nucleotide sequence?
        Left e2 -> Left e2
        Right (seq1, rest2) -> 
          case stringParser " " rest2 of -- space after the first sequence?
            Left e3 -> Left e3
            Right rest3 -> 
              case parseNucSeq rest3 of --query has a nucleotide sequence after space?
              Left e4 -> Left e4
              Right (seq2, _) -> 
                if null seq2
                then Left "Error: second nucleotide sequence missing."
                else Right (Concat seq1 seq2)

parseComplement :: String -> Either String Query
parseComplement input =
  case stringParser "complement " input of -- query starts with complement and a space?
    Left e1 -> Left e1
    Right rest1 ->
      case parseNucSeq rest1 of -- query has a nucleotide sequence?
        Left e2 -> Left e2
        Right (seq1, _) -> Right (Complement seq1)

parseTranscribe :: String -> Either String Query
parseTranscribe input =
  case stringParser "transcribe " input of -- query starts with transcribe and a space?
    Left e1 -> Left e1
    Right rest1 ->
      case parseNucSeq rest1 of -- query has a nucleotide sequence?
        Left e2 -> Left e2
        Right (seq1, _) -> Right (Transcribe seq1)















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
