module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      --stateTransition
    ) where

data Query = Concat Operand Operand
           | FMotif Operand Operand
           | Complement Operand
           | Transcribe Operand
           | Mutate Operand Integer
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
-- >>> parseNucSeq "A AG"

parseNucSeq :: Parser [Nucleotide]
parseNucSeq [] = Right ([], [])
parseNucSeq (h:t) = case parseNucleotide h of
    Left _ -> Right ([], h:t)  -- Stop parsing and return remaining input if not a nucleotide
    Right nucleotide -> case parseNucSeq t of
        Left err -> Left err
        Right (nucSeq, remainder) -> Right (nucleotide : nucSeq, remainder)



-- >>> parseConcat "concat AGGT GT"
-- Right (Concat (Sequence [A,G,G,T]) (Sequence [G,T]),"")


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


or5 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or5 p1 p2 p3 p4 p5 input =
  case p1 input of
    Right result -> Right result
    Left _ -> case p2 input of
      Right result -> Right result
      Left _ -> case p3 input of
        Right result -> Right result
        Left _ -> case p4 input of
          Right result -> Right result
          Left _ -> p5 input


-- >>> parseQuery "complement GGT"
-- Right (Complement (Sequence [G,G,T]),"")

parseQuery :: Parser Query
parseQuery input = 
  if take 1 input == "("
  then case parseParenthesizedQuery input of
         Left err -> Left err
         Right (query, rest) -> Right (query, rest)
  else or5 parseConcat parseFMotif parseComplement parseTranscribe parseMutate input

parseParenthesizedQuery :: Parser Query
parseParenthesizedQuery input = 
  case parseChar '(' input of
    Left err -> Left err
    Right (_, rest1) -> case parseQuery rest1 of
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





data State = State {
  nucleotideSequence :: String
} deriving (Show, Eq)

-- | Initial state of the program.
emptyState :: State
emptyState = State { nucleotideSequence = "" }

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
