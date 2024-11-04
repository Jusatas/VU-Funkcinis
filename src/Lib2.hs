{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition
    ) where

data Query = Concat Operand Operand
           | FMotif Operand Operand
           | Complement Operand
           | Transcribe Operand
           | Mutate Operand Integer
           | ViewCommand
           deriving (Show, Eq)


data Operand = Sequence [Nucleotide]
             | NestedQuery Query
             deriving (Show, Eq)


data Nucleotide = A | T | U | C | G
  deriving (Show, Eq)

type Parser a = String -> Either String (a, String)

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c (h:t)
  | c == h    = Right (c, t)
  | otherwise = Left ("|" ++ [c] ++ "| is not the first element of " ++ (h:t))

parseString :: String -> Parser String
parseString [] input = Right ([], input)
parseString (h:t) input = 
  case parseChar h input of
    Left err -> Left err
    Right (_, rest) -> 
      case parseString t rest of
        Left err -> Left err
        Right (parsed, remaining) -> Right (h:parsed, remaining)

-- <integer> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | <integer> <integer>
parseInt :: Parser Integer
parseInt [] = Left "Error: no digits found."
parseInt input = parseDigits 0 input

parseDigits :: Integer -> String -> Either String (Integer, String)
parseDigits acc [] = Right (acc, [])  -- No more input; return the accumulated integer and an empty remainder
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
  | otherwise = Right (acc, h:t)  -- Return the accumulated integer and the remaining unparsed input

-- <nucleotide> ::= "A" | "T" | "C" | "G"
parseNucleotide :: Char -> Either String Nucleotide
parseNucleotide 'A' = Right A
parseNucleotide 'T' = Right T
parseNucleotide 'U' = Right U
parseNucleotide 'C' = Right C
parseNucleotide 'G' = Right G
parseNucleotide _   = Left "Error: invalid nucleotide."

parseNucSeq :: Parser [Nucleotide]
parseNucSeq [] = Right ([], [])
parseNucSeq (h:t) = case parseNucleotide h of
    Left _ -> Right ([], h:t)  -- Stop parsing and return remaining input if not a nucleotide
    Right nucleotide -> case parseNucSeq t of
        Left err -> Left err
        Right (nucSeq, remainder) -> Right (nucleotide : nucSeq, remainder)

parseConcat :: Parser Query
parseConcat input = 
  case parseString "concat " input of
    Left err -> Left err
    Right (_, rest1) -> case parseOperand rest1 of
      Left err -> Left err
      Right (op1, rest2) -> case parseChar ' ' rest2 of
        Left err -> Left err
        Right (_, rest3) -> case parseOperand rest3 of
          Left err -> Left err
          Right (op2, remaining) -> Right (Concat op1 op2, remaining)

-- <operation> ::= "fmotif" <operand> <operand>
parseFMotif :: Parser Query
parseFMotif input =
  case parseString "fmotif " input of
    Left err -> Left err
    Right (_, rest1) -> case parseOperand rest1 of
      Left err -> Left err
      Right (op1, rest2) -> case parseChar ' ' rest2 of
        Left err -> Left err
        Right (_, rest3) -> case parseOperand rest3 of
          Left err -> Left err
          Right (op2, remaining) -> Right (FMotif op1 op2, remaining)

-- <operation> ::= "complement" <operand>
parseComplement :: Parser Query
parseComplement input =
  case parseString "complement " input of
    Left err -> Left err
    Right (_, rest) -> case parseOperand rest of
      Left err -> Left err
      Right (op, remaining) -> Right (Complement op, remaining)

-- <operation> ::= "transcribe" <operand>
parseTranscribe :: Parser Query
parseTranscribe input =
  case parseString "transcribe " input of
    Left err -> Left err
    Right (_, rest) -> case parseOperand rest of
      Left err -> Left err
      Right (op, remaining) -> Right (Transcribe op, remaining)

-- <operation> ::= "mutate" <operand> <percentage>
parseMutate :: Parser Query
parseMutate input =
  case parseString "mutate " input of
    Left err -> Left err
    Right (_, rest1) -> case parseOperand rest1 of
      Left err -> Left err
      Right (op, rest2) -> case parseString " " rest2 of
        Left err -> Left err
        Right (_, rest3) -> case parseInt rest3 of
          Left err -> Left err
          Right (int, remaining) -> Right (Mutate op int, remaining)

parseView :: Parser Query
parseView input =
  if take 4 input == "view"
  then Right (ViewCommand, drop 4 input)
  else Left "Expected 'view' for ViewCommand"



or6 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or6 p1 p2 p3 p4 p5 p6 input =
  case p1 input of
    Right result -> Right result
    Left _ -> case p2 input of
      Right result -> Right result
      Left _ -> case p3 input of
        Right result -> Right result
        Left _ -> case p4 input of
          Right result -> Right result
          Left _ -> case p5 input of
            Right result -> Right result
            Left _ -> p6 input


parseQuery :: String -> Either String Query
parseQuery input = 
  case parseParenthesizedOrRegularQuery input of
    Right (query, _) -> Right query
    Left _ -> Left "Error: command doesn't match anything from query."

-- Helper function to handle both regular and parenthesized queries
parseParenthesizedOrRegularQuery :: Parser Query
parseParenthesizedOrRegularQuery input =
  if take 1 input == "("
  then parseParenthesizedQuery input
  else or6 parseConcat parseFMotif parseComplement parseTranscribe parseMutate parseView input



parseParenthesizedQuery :: Parser Query
parseParenthesizedQuery input = 
  case parseChar '(' input of
    Left err -> Left err
    Right (_, rest1) -> case parseParenthesizedOrRegularQuery rest1 of
      Left err -> Left err
      Right (query, rest2) -> case parseChar ')' rest2 of
        Left err -> Left err
        Right (_, remaining) -> Right (query, remaining)


parseOperand :: Parser Operand
parseOperand input
  | take 1 input == "(" = 
      case parseParenthesizedQuery input of
        Left err -> Left err
        Right (query, rest) -> Right (NestedQuery query, rest)
  | otherwise = case parseNucSeq input of
      Left err -> Left err
      Right (nucleotides, rest) -> Right (Sequence nucleotides, rest)


-- >>> parseQuery "mutate (concat CC (complement G)) 50"
-- Right (Mutate (NestedQuery (Concat (Sequence [C,C]) (NestedQuery (Complement (Sequence [G]))))) 50,"")

-- >>> parseQuery "complement (mutate AG 30)"
-- Right (Complement (NestedQuery (Mutate (Sequence [A,G]) 30)),"")


-- >>> parseQuery "mutate (complement (fmotif A (concat T (complement G)))) 15"
-- Right (Mutate (NestedQuery (Complement (NestedQuery (FMotif (Sequence [A]) (NestedQuery (Concat (Sequence [T]) (NestedQuery (Complement (Sequence [G]))))))))) 15,"")

-- >>> parseQuery "concat (mutate (fmotif CC GG) 25) (complement (transcribe T))"
-- Right (Concat (NestedQuery (Mutate (NestedQuery (FMotif (Sequence [C,C]) (Sequence [G,G]))) 25)) (NestedQuery (Complement (NestedQuery (Transcribe (Sequence [T]))))),"")

-- >>> parseQuery "complement GGT"
-- Right (Complement (Sequence [G,G,T]),"")




data State = State
  { nucleotideSequence :: [Nucleotide]  -- Sequence of nucleotides
  , commandHistory :: [Query]           -- History of executed commands
  } deriving (Show, Eq)

-- | Initial state of the program
emptyState :: State
emptyState = State
  { nucleotideSequence = [],
    commandHistory = []
  }



viewState :: State -> String
viewState state =
  "Nucleotide Sequence: " ++ nucleotidesToString (nucleotideSequence state) ++ "\n" ++
  "Command History:\n" ++ unlines (map show (commandHistory state))


-- Helper function to convert Nucleotide type to Char
nucleotideToChar :: Nucleotide -> Char
nucleotideToChar A = 'A'
nucleotideToChar T = 'T'
nucleotideToChar U = 'U'
nucleotideToChar C = 'C'
nucleotideToChar G = 'G'

charToNucleotide :: Char -> Nucleotide
charToNucleotide 'A' = A
charToNucleotide 'T' = T
charToNucleotide 'U' = U
charToNucleotide 'C' = C
charToNucleotide 'G' = G
charToNucleotide _   = error "Invalid nucleotide character"

-- Helper function to convert a list of Nucleotides to a String
nucleotidesToString :: [Nucleotide] -> String
nucleotidesToString = map nucleotideToChar

-- State transition function
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
  Concat op1 op2 ->
    case (extractSequence state op1, extractSequence state op2) of
      (Right s1, Right s2) ->
        let newSeq = nucleotideSequence state ++ s1 ++ s2
            newState = state { nucleotideSequence = newSeq, commandHistory = commandHistory state ++ [query] }
        in Right (Just "Sequences concatenated.", newState)
      (Left err, _) -> Left err
      (_, Left err) -> Left err

  FMotif op1 motifOp ->
    case (extractSequence state op1, extractSequence state motifOp) of
      (Right s1, Right motif) ->
        let found = containsMotif (nucleotidesToString s1) (nucleotidesToString motif)
            message = if found then "Motif found in sequence." else "Motif not found."
            newState = state { commandHistory = commandHistory state ++ [query] }
        in Right (Just message, newState)
      (Left err, _) -> Left err
      (_, Left err) -> Left err

  Complement op ->
    case extractSequence state op of
      Right seq' ->
        case complementSequence (nucleotidesToString seq') of
          Left err -> Left err
          Right complemented ->
            let newState = state { nucleotideSequence = map charToNucleotide complemented, commandHistory = commandHistory state ++ [query] }
            in Right (Just "Sequence complemented.", newState)
      Left err -> Left err

  Transcribe op ->
    case extractSequence state op of
      Right seq' ->
        let transcribed = map charToNucleotide (transcribeSequence (nucleotidesToString seq'))
            newState = state { nucleotideSequence = transcribed, commandHistory = commandHistory state ++ [query] }
        in Right (Just "Sequence transcribed to RNA.", newState)
      Left err -> Left err

  Mutate op percentage ->
    case extractSequence state op of
      Right seq' ->
        let mutated = map charToNucleotide (mutate (nucleotidesToString seq') percentage)
            newState = state { nucleotideSequence = mutated, commandHistory = commandHistory state ++ [query] }
        in Right (Just "Sequence mutated.", newState)
      Left err -> Left err

  ViewCommand ->
    Right (Just $ "Current State:\n" ++ viewState state, state)

-- Helper function to extract sequences from operands
extractSequence :: State -> Operand -> Either String [Nucleotide]
extractSequence _ (Sequence s) = Right s
extractSequence state (NestedQuery nestedQuery) = 
  case stateTransition state nestedQuery of
    Left err -> Left err
    Right (_, newState) -> Right (nucleotideSequence newState)


-- | Test for stateTransition function
-- >>> let initState = State { nucleotideSequence = [A, T, G], commandHistory = [] }
-- >>> stateTransition initState (Concat (Sequence [C, G]) (Sequence [T, A]))
-- Right (Just "Sequences concatenated.",State {nucleotideSequence = [A,T,G,C,G,T,A], commandHistory = [Concat (Sequence [C,G]) (Sequence [T,A])]})



-- Right (Just "Sequences concatenated.", State {nucleotideSequence = [A, T, G, C, G, T, A], commandHistory = [Concat (Sequence [C, G]) (Sequence [T, A])]})
--stateTransition :: State -> Query -> Either String (Maybe String, State)



containsMotif :: String -> String -> Bool
containsMotif [] _ = False
containsMotif seq1 seq2 =
  if startsWith seq1 seq2
  then True
  else containsMotif (tail seq1) seq2

-- >>> startsWith "ATGC" "ATG"
-- True
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
mutate seq' percentage =
  let numToMutate = fromIntegral (length seq') * percentage `div` 100
      (toMutate, rest) = splitAt (fromInteger numToMutate) seq'
  in map mutateNucleotide toMutate ++ rest

-- | Mutates a single nucleotide to another random nucleotide (simplified).
mutateNucleotide :: Char -> Char
mutateNucleotide 'A' = 'T'
mutateNucleotide 'T' = 'A'
mutateNucleotide 'C' = 'G'
mutateNucleotide 'G' = 'C'
mutateNucleotide nucleotide = nucleotide

-- Returns 1 if found, 0 if not found.
fmotif :: String -> String -> Int
fmotif [] _ = 0  -- If sequence is empty, return 0
fmotif _ [] = 0  -- If motif is empty, return 0
fmotif seq1 motif
    | startsWith seq1 motif = 1  -- Found at the start
    | otherwise = fmotif (tail seq1) motif  -- Recursively check the rest
