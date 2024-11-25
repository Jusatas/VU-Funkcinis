{-# LANGUAGE InstanceSigs #-}
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
import Control.Concurrent.STM(STM, TVar)
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
marshallState _ = error "Not implemented 4"

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements _ = error "Not implemented 5"

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
stateTransition _ _ ioChan = return $ Left "Not implemented 6"
