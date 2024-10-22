module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition
    ) where

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
parseDigits acc [] = Right (acc)
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
  | otherwise = Right (acc) -- Stop when a non-digit character is found
-- <nucleotide> ::= "A" | "T" | "C" | "G"
parseNucleotide :: Char -> Either String Char
parseNucleotide 'A' = Right 'A'
parseNucleotide 'T' = Right 'T'
parseNucleotide 'U' = Right 'U'
parseNucleotide 'C' = Right 'C'
parseNucleotide 'G' = Right 'G'
parseNucleotide _   = Left "Error: invalid nucleotide."

-- | Parses a nucleotide sequence (string of valid nucleotides).
-- >>> parseNucSeq "ATG"
-- Right ("ATG","")

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

-- >>> parseQuery "mutate GGC (fmotif GGCCGC GC)"
-- Right "GGC"



-- Right "GCC"
-- >>> parseQuery "mutate (concat CC (complement G)) 50"
-- Right "GCC"

parseQuery :: String -> Either String String
parseQuery input =
    case findInnermostParentheses input of
        Nothing -> executeCommand input  -- No parentheses, try to execute the command
        Just (start, end) ->
            -- Extract content inside the parentheses
            let inside = extractSubstring input (start + 1) (end - 1)
                -- Recursively parse the content inside the parentheses
                result = parseQuery inside
            in case result of
                Left err -> Left err
                Right parsedResult ->
                    -- Replace the evaluated result back in the string and continue parsing
                    let newInput = replaceSubstring input start end parsedResult
                    in parseQuery newInput  -- Continue processing the updated input



findInnermostParentheses :: String -> Maybe (Int, Int)
findInnermostParentheses input = findDeepest 0 (-1, -1) 0
  where
    findDeepest _ (start, end) idx
        | idx >= length input = if start /= -1 then Just (start, end) else Nothing
        | otherwise =
            let char = input !! idx
            in case char of
                '(' -> findDeepest 1 (idx, -1) (idx + 1)
                ')' -> if start /= -1 && end == -1
                       then Just (start, idx)
                       else findDeepest 0 (-1, -1) (idx + 1)
                _   -> findDeepest 0 (start, end) (idx + 1)

extractSubstring :: String -> Int -> Int -> String
extractSubstring input start end = extractHelper input start end 0

extractHelper :: String -> Int -> Int -> Int -> String
extractHelper [] _ _ _ = [] 
extractHelper (x:xs) start end currentIndex
    | currentIndex > end = [] 
    | currentIndex >= start = x : extractHelper xs start end (currentIndex + 1)
    | otherwise = extractHelper xs start end (currentIndex + 1)


-- Replace a part of the string between start and end with the result
replaceSubstring :: String -> Int -> Int -> String -> String
replaceSubstring input start end result =
    let before = take start input
        after  = drop (end + 1) input
    in before ++ result ++ after

executeCommand :: String -> Either String String
executeCommand input =
    case parseCommand input of
        Left err -> Left err
        Right query -> evaluateQuery query

parseCommand :: String -> Either String Query
parseCommand input =
    case parseConcat input of
        Right result -> Right result
        Left _ ->
            case parseFMotif input of
                Right result -> Right result
                Left _ ->
                    case parseComplement input of
                        Right result -> Right result
                        Left _ ->
                            case parseTranscribe input of
                                Right result -> Right result
                                Left _ ->
                                    case parseMutate input of
                                        Right result -> Right result
                                        Left _ -> Left "Error: Unknown command."

evaluateQuery :: Query -> Either String String
evaluateQuery (Complement seq') = complementSequence seq'  -- Calls complementSequence
evaluateQuery (Transcribe seq') = Right (transcribeSequence seq')  -- Calls transcribeSequence
evaluateQuery (Mutate seq' percentage) = Right (mutate seq' percentage)  -- Mutates the sequence
evaluateQuery (Concat seq1 seq2) = Right (seq1 ++ seq2)  -- Concatenates two sequences
evaluateQuery (FMotif seq1 motif) =
    let result = fmotif seq1 motif
    in Right (show result)


--evaluateQuery (FMotif seq1 motif) = 
--    if motif `elem` subsequences seq1
--    then Right "Motif found"
--    else Right "Motif not found"

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
  where
    -- Check if seq1 starts with the motif
    startsWith :: String -> String -> Bool
    startsWith [] _ = False
    startsWith _ [] = True
    startsWith (x:xs) (y:ys) = (x == y) && startsWith xs ys
