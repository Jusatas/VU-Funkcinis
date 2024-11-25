{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Concurrent ( Chan )
import Control.Concurrent.STM(STM, TVar, readTVar, atomically, writeTVar)
import qualified Lib2
import Debug.Trace (trace)

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop _ = do
  return $ error "Not implemented 1"

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input = case words input of
    ("load":rest) -> Right (LoadCommand, unwords rest) -- Matches "load" command
    ("save":rest) -> Right (SaveCommand, unwords rest) -- Matches "save" command
    _ -> case parseStatements input of -- Delegates to `parseStatements` for other cases
           Right (stmts, rest) -> Right (StatementCommand stmts, rest)
           Left err -> Left err

-- >>> parseCommand "save output.txt"
-- Right (SaveCommand,"output.txt")

-- >>> parseCommand "concat CG G"
-- Right (StatementCommand (Single (Concat (Sequence [C,G]) (Sequence [G]))),"")

-- >>> parseCommand "BEGIN concat CG G; complement G; mutate CCCGGAGAT 75; END"
-- Right (StatementCommand (Batch [Concat (Sequence [C,G]) (Sequence [G]),Complement (Sequence [G]),Mutate (Sequence [C,C,C,G,G,A,G,A,T]) 75]),"")


-- | Parses Statement.
parseStatements :: String -> Either String (Statements, String)
parseStatements input
    | take 5 input == "BEGIN" = parseBatch (drop 5 input)
    | otherwise =
        let trimmedInput = dropWhile isWhitespace input
        in case trace ("Calling Lib2.parseQuery with: " ++ show trimmedInput) Lib2.parseQuery trimmedInput of
            Right query -> Right (Single query, "")
            Left err -> Left err

parseBatch :: String -> Either String (Statements, String)
parseBatch inp = case parseQueries [] (dropWhile isWhitespace inp) of
    Right (queries, rest) -> Right (Batch queries, rest)
    Left err -> Left err

parseQueries :: [Lib2.Query] -> String -> Either String ([Lib2.Query], String)
parseQueries acc input'
    | take 3 input' == "END" = Right (reverse acc, drop 3 input') -- Stop
    | otherwise =
        let (queryStr, rest) = break (== ';') input' -- Split on the semicolon
        in if null queryStr
           then Left "Empty query before semicolon"
           else case Lib2.parseQuery (trim queryStr) of
               Right query ->
                   parseQueries (query : acc) (dropWhile isWhitespace (drop 1 rest)) -- Skip semicolon
               Left err -> Left err

-- Trims whitespace from both ends of a string.
trim :: String -> String
trim str =
    let withoutLeading = dropWhile isWhitespace str
        withoutTrailing = reverse (dropWhile isWhitespace (reverse withoutLeading))
    in withoutTrailing

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\n', '\t', ';']









-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state = Batch queries
  where
    currentSeqQuery = if null (Lib2.nucleotideSequence state)
                     then []
                     else [Lib2.CreateSeq (Lib2.nucleotideSequence state) "current"]
    
    -- Create queries for all named sequences
    namedSeqQueries = map (\(name, seq) -> Lib2.CreateSeq seq name) 
                         (Lib2.namedSequences state)
    
    -- Combine all queries
    queries = currentSeqQuery ++ namedSeqQueries

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single q) = renderQuery q
renderStatements (Batch qs) = "BEGIN\n" ++ 
                             concatMap (\q -> renderQuery q ++ ";\n") qs ++ 
                             "END\n"

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.CreateSeq nucleotides name) = 
    "createSeq " ++ nucleotidesToString nucleotides ++ " " ++ name
renderQuery (Lib2.SaveTo name query) = 
    "saveTo " ++ name ++ " " ++ renderQuery query
renderQuery (Lib2.Concat op1 op2) = 
    "concat " ++ renderOperand op1 ++ " " ++ renderOperand op2
renderQuery (Lib2.Complement op) = 
    "complement " ++ renderOperand op
renderQuery (Lib2.Transcribe op) = 
    "transcribe " ++ renderOperand op
renderQuery (Lib2.Mutate op n) = 
    "mutate " ++ renderOperand op ++ " " ++ show n
renderQuery Lib2.ViewCommand = "view"
renderQuery (Lib2.DeleteSeq name) = "deleteSeq " ++ name
renderQuery (Lib2.FMotif op1 op2) = 
    "fmotif " ++ renderOperand op1 ++ " " ++ renderOperand op2

renderOperand :: Lib2.Operand -> String
renderOperand (Lib2.Sequence nucleotides) = nucleotidesToString nucleotides
renderOperand (Lib2.NestedQuery query) = "(" ++ renderQuery query ++ ")"
renderOperand (Lib2.NamedSequence name) = name

nucleotidesToString :: [Lib2.Nucleotide] -> String
nucleotidesToString = map nucleotideToChar
  where
    nucleotideToChar Lib2.A = 'A'
    nucleotideToChar Lib2.T = 'T'
    nucleotideToChar Lib2.U = 'U'
    nucleotideToChar Lib2.C = 'C'
    nucleotideToChar Lib2.G = 'G'






















-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition stateVar command _ = case command of
    LoadCommand -> return $ Left "Load not implemented yet"
    SaveCommand -> return $ Left "Save not implemented yet"
    StatementCommand stmts -> atomically $ do
        state <- readTVar stateVar
        case executeStatements state stmts of
            Right (msg, newState) -> do
                writeTVar stateVar newState
                return $ Right msg
            Left err -> return $ Left err

executeStatements :: Lib2.State -> Statements -> Either String (Maybe String, Lib2.State)
executeStatements state (Single query) = Lib2.stateTransition state query
executeStatements state (Batch queries) = 
    foldr combineBatchResults (Right (Nothing, state)) queries
  where
    combineBatchResults query acc = case acc of
        Left err -> Left err
        Right (_, curState) -> case Lib2.stateTransition curState query of
            Left err -> Left err
            Right (msg, newState) -> Right (msg, newState)

        
-- >>> marshallState Lib2.emptyState
-- Batch []

-- >>> let testState = Lib2.emptyState { Lib2.nucleotideSequence = [Lib2.A, Lib2.T, Lib2.C], Lib2.namedSequences = [("test1", [Lib2.G, Lib2.C]), ("test2", [Lib2.A, Lib2.A])]}
-- >>> marshallState testState
-- Batch [CreateSeq [A,T,C] "current",CreateSeq [G,C] "test1",CreateSeq [A,A] "test2"]