
module Lib2
    ( Query(..)
    , Operand(..)
    , Nucleotide(..)
    , parseQuery
    , State(..)
    , emptyState
    , stateTransition
    ) where

data Query = Concat Operand Operand
           | FMotif Operand Operand
           | Complement Operand
           | Transcribe Operand
           | Mutate Operand Integer
           | ViewCommand
           | CreateSeq [Nucleotide] String
           | DeleteSeq String
           | SaveTo String Query
           deriving (Show, Eq)



data Operand = Sequence [Nucleotide]
             | NestedQuery Query
             | NamedSequence String
             deriving (Show, Eq)


data Nucleotide = A | T | U | C | G
  deriving (Show, Eq)

type Parser a = String -> Either String (a, String)

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c (h:t)
  | c == h    = Right (c, t)
  | otherwise = Left ("|" ++ [c] ++ "| is not the first element of " ++ (h:t))


-- <string> ::= <character> <string> | <character>
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

-- <sequence> ::= <nucleotide> | <nucleotide> <sequence>
parseNucSeq :: Parser [Nucleotide]
parseNucSeq [] = Right ([], [])
parseNucSeq (h:t) = case parseNucleotide h of
    Left _ -> Right ([], h:t)  -- Stop parsing and return remaining input if not a nucleotide
    Right nucleotide -> case parseNucSeq t of
        Left err -> Left err
        Right (nucSeq, remainder) -> Right (nucleotide : nucSeq, remainder)

-- <operation> ::= "concat" <operand> <operand>
parseConcat :: Parser Query
parseConcat input =
  case and4 (parseString "concat ") parseOperand (parseChar ' ') parseOperand input of
    Left err -> Left err
    Right ((_, op1, _, op2), remaining) -> Right (Concat op1 op2, remaining)

-- <operation> ::= "fmotif" <operand> <operand>
parseFMotif :: Parser Query
parseFMotif input =
  case and4 (parseString "fmotif ") parseOperand (parseChar ' ') parseOperand input of
    Left err -> Left err
    Right ((_, op1, _, op2), remaining) -> Right (FMotif op1 op2, remaining)

-- <operation> ::= "complement" <operand>
parseComplement :: Parser Query
parseComplement input =
  case and2 (parseString "complement ") parseOperand input of
    Left err -> Left err
    Right ((_, op), remaining) -> Right (Complement op, remaining)

-- <operation> ::= "transcribe" <operand>
parseTranscribe :: Parser Query
parseTranscribe input =
  case and2 (parseString "transcribe ") parseOperand input of
    Left err -> Left err
    Right ((_, op), remaining) -> Right (Transcribe op, remaining)

-- <operation> ::= "mutate" <operand> <percentage>
parseMutate :: Parser Query
parseMutate input =
  case and4 (parseString "mutate ") parseOperand (parseChar ' ') parseInt input of
    Left err -> Left err
    Right ((_, op, _, int), remaining) -> Right (Mutate op int, remaining)

-- <operation> ::= "deleteSeq" <string>
parseDeleteSeq :: Parser Query
parseDeleteSeq input =
  case and2 (parseString "deleteSeq ") parseName input of
    Left err -> Left err
    Right ((_, name), remaining) -> Right (DeleteSeq name, remaining)

-- <operation> ::= "createSeq" <operand> <string>
parseCreateSeq :: String -> Either String (Query, String)
parseCreateSeq input =
  case and4 (parseString "createSeq ") parseNucSeq (parseString " ") parseName input of
    Left err -> Left err
    Right ((_, nucleotideSeq, _, name), remaining) -> Right (CreateSeq nucleotideSeq name, remaining)

parseSaveTo :: Parser Query
parseSaveTo input = 
  case parseString "saveTo " input of
    Left err -> Left err
    Right (_, rest1) -> 
      case parseAlphaNumString rest1 of
        Left err -> Left err
        Right (name, rest2) ->
          case parseChar ' ' rest2 of
            Left err -> Left err
            Right (_, rest3) -> 
              case parseAnyQuery rest3 of  -- Use parseAnyQuery which handles both parenthesized and regular queries
                Left err -> Left $ "Failed to parse query in saveTo: " ++ err
                Right (query, remaining) -> Right (SaveTo name query, remaining)


parseName :: Parser String
parseName "" = Left "Error: Missing name."
parseName input = Right (input, "")


-- <operation> ::= "view"
parseView :: Parser Query
parseView input =
  if take 4 input == "view"
  then Right (ViewCommand, drop 4 input)
  else Left "Expected 'view' for ViewCommand"

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input =
  case p1 input of
    Left err -> Left err
    Right (result1, rest) ->
      case p2 rest of
        Left err -> Left err
        Right (result2, remaining) -> Right ((result1, result2), remaining)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 p1 p2 p3 input =
  case p1 input of
    Left err -> Left err
    Right (result1, rest1) ->
      case p2 rest1 of
        Left err -> Left err
        Right (result2, rest2) ->
          case p3 rest2 of
            Left err -> Left err
            Right (result3, remaining) -> Right ((result1, result2, result3), remaining)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
and4 p1 p2 p3 p4 input =
  case p1 input of
    Left err -> Left err
    Right (result1, rest1) ->
      case p2 rest1 of
        Left err -> Left err
        Right (result2, rest2) ->
          case p3 rest2 of
            Left err -> Left err
            Right (result3, rest3) ->
              case p4 rest3 of
                Left err -> Left err
                Right (result4, remaining) -> Right ((result1, result2, result3, result4), remaining)

or2 :: Parser a -> Parser a -> Parser a
or2 p1 p2 input =
  case p1 input of
    Right result -> Right result
    Left _ -> p2 input

or3 :: Parser a -> Parser a -> Parser a -> Parser a
or3 p1 p2 p3 input =
  case p1 input of
    Right result -> Right result
    Left _ -> case p2 input of
      Right result -> Right result
      Left _ -> p3 input

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

or8 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or8 p1 p2 p3 p4 p5 p6 p7 p8 input =
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
            Left _ -> case p6 input of
              Right result -> Right result
              Left _ -> case p7 input of
                Right result -> Right result
                Left _ -> p8 input

or9 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or9 p1 p2 p3 p4 p5 p6 p7 p8 p9 input =
  case or8 p1 p2 p3 p4 p5 p6 p7 p8 input of
    Right result -> Right result
    Left _ -> p9 input

-- <operand> ::= <sequence> | <operation> | <namedsequence>
parseQuery :: String -> Either String Query
parseQuery input = 
  case parseAnyQuery input of
    Right (query, remaining) -> 
      if null remaining 
      then Right query 
      else Left $ "Error: unexpected trailing input: " ++ remaining
    Left err -> Left err

-- parses regular and parenthesized queries
parseAnyQuery :: Parser Query
parseAnyQuery ('(':rest) = parseParenthesizedQuery ('(':rest)
parseAnyQuery input = 
  or9 parseSaveTo parseConcat parseFMotif parseComplement parseTranscribe parseMutate parseView parseCreateSeq parseDeleteSeq input

parseParenthesizedQuery :: Parser Query
parseParenthesizedQuery input = 
  case and3 (parseChar '(') parseAnyQuery (parseChar ')') input of
    Left err -> Left err
    Right ((_, query, _), remaining) -> Right (query, remaining)

parseOperand :: Parser Operand
parseOperand ('(':rest) =
  case parseParenthesizedQuery ('(':rest) of
    Left err -> Left err
    Right (query, rest) -> Right (NestedQuery query, rest)
parseOperand input =
  or2
    parseNucleotideOperand
    parseNamedOperand
    input

parseNamedOperand :: Parser Operand
parseNamedOperand input =
    case parseAlphaNumString input of
      Left err -> Left err
      Right (name, remaining) -> Right (NamedSequence name, remaining)

parseNucleotideOperand :: Parser Operand
parseNucleotideOperand input =
    case parseNucSeq input of
      Right ([], _) -> Left "Empty nucleotide sequence"  -- Force it to try the name parser
      Right (nucleotides, rest) -> Right (Sequence nucleotides, rest)
      Left err -> Left err

-- New helper to parse alphanumeric strings (for sequence names)
parseAlphaNumString :: Parser String
parseAlphaNumString [] = Left "Error: Empty input for sequence name"
parseAlphaNumString input = parseAlphaNum [] input
  where
    parseAlphaNum acc (c:cs)
      | isAlphaNum c = parseAlphaNum (acc ++ [c]) cs
      | null acc = Left "Error: Invalid sequence name"
      | otherwise = Right (acc, c:cs)
    parseAlphaNum acc [] = Right (acc, [])
    
    isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

data State = State {
  nucleotideSequence :: [Nucleotide],
  commandHistory :: [Query],
  namedSequences :: [(String, [Nucleotide])]
} deriving (Show, Eq)

emptyState :: State
emptyState = State
  { nucleotideSequence = [],
    commandHistory = [],
    namedSequences = []
  }

viewState :: State -> String
viewState state =
  "------------------------------------------------------------------\n" ++
  "Nucleotide Sequence: " ++ nucleotidesToString (nucleotideSequence state) ++ "\n\n" ++
  "Saved Sequences:\n" ++ formatNamedSequences (namedSequences state)  ++ "\n\n\n" ++
  "Command History:\n" ++ unlines (map show (commandHistory state)) ++
  "------------------------------------------------------------------\n"

-- Helper function to format named sequences for display
formatNamedSequences :: [(String, [Nucleotide])] -> String
formatNamedSequences [] = "No saved sequences."
formatNamedSequences sequences = unlines $ map formatSequence sequences
  where
    formatSequence (name, nucleotides) = name ++ ": " ++ nucleotidesToString nucleotides


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

nucleotidesToString :: [Nucleotide] -> String
nucleotidesToString = map nucleotideToChar

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
  
  SaveTo name subQuery -> 
    case stateTransition state subQuery of
      Left err -> Left err
      Right (_, newState) -> 
        let newNamedSequences = (name, nucleotideSequence newState) : namedSequences state
            finalState = state { 
              namedSequences = newNamedSequences, 
              commandHistory = commandHistory state ++ [query] 
            }
        in Right (Just ("Sequence saved as " ++ name ++ "."), finalState)

  Concat op1 op2 ->
    case (extractSequence state op1, extractSequence state op2) of
      (Right s1, Right s2) ->
        let newSeq = concatSequences s1 s2
            newState = state { nucleotideSequence = newSeq, commandHistory = commandHistory state ++ [query] }
        in Right (Just "Sequences concatenated.", newState)
      (Left err, _) -> Left err
      (_, Left err) -> Left err

  FMotif op1 motifOp ->
    case (extractSequence state op1, extractSequence state motifOp) of
      (Right s1, Right motif) ->
        let index = fmotif (nucleotidesToString s1) (nucleotidesToString motif)
            message = if index > 0
                      then "Motif found at index: " ++ show index ++ "."
                      else "Motif not found."
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
    Right (Just $ "\nCurrent State:\n" ++ viewState state, state)

  CreateSeq sequence name ->
    let newNamedSequences = (name, sequence) : namedSequences state
        newState = state { namedSequences = newNamedSequences, commandHistory = commandHistory state ++ [query] }
    in Right (Just ("Sequence " ++ name ++ " created."), newState)

  DeleteSeq name ->
    let
    removeSequence [] = []
    removeSequence ((n, seq):xs) =
      if n == name
      then xs  -- Skip this item to remove it
      else (n, seq) : removeSequence xs  -- Keep the item and continue

    -- Get the new list of named sequences
    newNamedSequences = removeSequence (namedSequences state)

    -- Check if the sequence was actually found and removed
    in if length newNamedSequences == length (namedSequences state)
     then Left ("Error: Sequence " ++ name ++ " not found.")
     else
       let newState = state { namedSequences = newNamedSequences, commandHistory = commandHistory state ++ [query] }
       in Right (Just ("Sequence " ++ name ++ " deleted."), newState)




-- Helper function to extract sequences from operands
extractSequence :: State -> Operand -> Either String [Nucleotide]
extractSequence _ (Sequence s) = Right s
extractSequence state (NestedQuery nestedQuery) =
  case stateTransition state nestedQuery of
    Left err -> Left err
    Right (_, newState) -> Right (nucleotideSequence newState)
extractSequence state (NamedSequence name) =
  case lookup name (namedSequences state) of
    Nothing -> Left $ "Error: Sequence '" ++ name ++ "' not found."
    Just seq -> Right seq

concatSequences :: [Nucleotide] -> [Nucleotide] -> [Nucleotide]
concatSequences seq1 seq2 = seq1 ++ seq2

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
complementNucleotide 'U' = Right 'A'
complementNucleotide _ = Left "Error: Invalid nucleotide for complement."

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

mutateNucleotide :: Char -> Char
mutateNucleotide 'A' = 'T'
mutateNucleotide 'T' = 'A'
mutateNucleotide 'C' = 'G'
mutateNucleotide 'G' = 'C'
mutateNucleotide nucleotide = nucleotide

fmotif :: String -> String -> Int
fmotif [] _ = 0  -- If sequence is empty, return 0
fmotif _ [] = 0  -- If motif is empty, return 0
fmotif seq1 motif = fmotifHelper seq1 motif 1  -- Start index at 1

fmotifHelper :: String -> String -> Int -> Int
fmotifHelper [] _ _ = 0  -- If sequence is empty, return 0
fmotifHelper seq1 motif index
    | startsWith seq1 motif = index  -- Found motif, return index
    | otherwise = fmotifHelper (tail seq1) motif (index + 1)  -- Checkrest, increment index

startsWith :: String -> String -> Bool
startsWith [] _ = False
startsWith _ [] = True
startsWith (x:xs) (y:ys) = (x == y) && startsWith xs ys
