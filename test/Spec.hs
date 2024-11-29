{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC

import Lib2 qualified
import Lib2 (Query(..), Operand(..), Nucleotide(..), State(..), emptyState)
import Lib3 qualified
import Lib3 (Statements(..))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 Tests"
  [ testGroup "Basic Query Parsing"
    [ testCase "Parse simple complement" $
        Lib2.parseQuery "complement GGT" @?= Right (Complement (Sequence [G,G,T]))
    , testCase "Parse simple concat" $
        Lib2.parseQuery "concat AT GC" @?= Right (Concat (Sequence [A,T]) (Sequence [G,C]))
    , testCase "Parse simple transcribe" $
        Lib2.parseQuery "transcribe ATGC" @?= Right (Transcribe (Sequence [A,T,G,C]))
    , testCase "Parse simple mutate" $
        Lib2.parseQuery "mutate ATGC 50" @?= Right (Mutate (Sequence [A,T,G,C]) 50)
    ]
  , testGroup "Complex Nested Query Parsing"
    [ testCase "Parse nested complement in mutate" $
        Lib2.parseQuery "complement (mutate AG 30)" @?=
        Right (Complement (NestedQuery (Mutate (Sequence [A,G]) 30)))
    , testCase "Parse deeply nested query" $
        Lib2.parseQuery "mutate (complement (fmotif A (concat T (complement G)))) 15" @?=
        Right (Mutate (NestedQuery (Complement (NestedQuery (FMotif
          (Sequence [A])
          (NestedQuery (Concat (Sequence [T]) (NestedQuery (Complement (Sequence [G])))))
        )))) 15)
    ]
  , testGroup "State Transitions"
    [ testCase "Simple concat state transition" $
        case Lib2.stateTransition emptyState (Concat (Sequence [A,T]) (Sequence [G,C])) of
          Right (msg, newState) -> do
            msg @?= Just "Sequences concatenated."
            nucleotideSequence newState @?= [A,T,G,C]
          Left err -> error $ "Test failed: " ++ err
    , testCase "Simple complement state transition" $
        case Lib2.stateTransition emptyState (Complement (Sequence [A,T,G,C])) of
          Right (msg, newState) -> do
            msg @?= Just "Sequence complemented."
            nucleotideSequence newState @?= [T,A,C,G]
          Left err -> error $ "Test failed: " ++ err
    ]
  , testGroup "Named Sequence Operations"
    [ testCase "Create and retrieve named sequence" $ do
        let createCmd = CreateSeq [A,T,G,C] "test1"
            state1 = case Lib2.stateTransition emptyState createCmd of
              Right (_, newState) -> newState
              Left err -> error $ "Test failed: " ++ err
        case lookup "test1" (namedSequences state1) of
          Just seq -> seq @?= [A,T,G,C]
          Nothing -> error "Test failed: Named sequence not found"
    , testCase "Delete named sequence" $ do
        let createCmd = CreateSeq [A,T,G,C] "test2"
            state1 = case Lib2.stateTransition emptyState createCmd of
              Right (_, newState) -> newState
              Left err -> error $ "Test failed: " ++ err
            state2 = case Lib2.stateTransition state1 (DeleteSeq "test2") of
              Right (_, newState) -> newState
              Left err -> error $ "Test failed: " ++ err
        namedSequences state2 @?= []
    ]
  , testGroup "Error Cases"
    [ testCase "Missing operand in concat" $
        case Lib2.parseQuery "concat AT" of
          Left _ -> return ()
          Right _ -> error "Test failed: Should have rejected incomplete concat"
    , testCase "Delete non-existent sequence" $
        case Lib2.stateTransition emptyState (DeleteSeq "nonexistent") of
          Left _ -> return ()
          Right _ -> error "Test failed: Should have rejected deleting non-existent sequence"
    ]
  ]

genNucleotide :: Gen Lib2.Nucleotide
genNucleotide = QC.elements [Lib2.A, Lib2.T, Lib2.C, Lib2.G, Lib2.U]

genNonEmptyNucleotideSequence :: Gen [Lib2.Nucleotide]
genNonEmptyNucleotideSequence = do
  len <- choose (1, 10)
  vectorOf len genNucleotide

genOperand :: Gen Lib2.Operand
genOperand = Lib2.Sequence <$> genNonEmptyNucleotideSequence

genQuery :: Gen Lib2.Query
genQuery = QC.oneof
  [ Lib2.CreateSeq <$> genNonEmptyNucleotideSequence <*> pure "test"
  , genSaveTo
  , Lib2.Concat <$> genOperand <*> genOperand
  , Lib2.Complement <$> genOperand
  , Lib2.Transcribe <$> genOperand
  , genMutate
  , pure Lib2.ViewCommand
  , Lib2.FMotif <$> genOperand <*> genOperand
  ]

genSaveTo :: Gen Lib2.Query
genSaveTo = do
  query <- genQuery
  return $ Lib2.SaveTo "test" query

genMutate :: Gen Lib2.Query
genMutate = do
  operand <- genOperand
  mutationRate <- choose (1, 100)
  return $ Lib2.Mutate operand mutationRate

genStatements :: Gen Lib3.Statements
genStatements = QC.oneof
  [ Lib3.Single <$> genQuery
  , Lib3.Batch <$> listOf1 genQuery
  ]

instance Arbitrary Lib3.Statements where
  arbitrary = genStatements

propertyTests :: TestTree
propertyTests = testGroup "State Preservation Tests"
  [ QC.testProperty "Render and parse statements preserves state and consumes input" $
      \statements ->
        let renderedStatements = Lib3.renderStatements statements
            parsedResult = Lib3.parseStatements renderedStatements
        in QC.counterexample
            (unlines
              [ "Original statements: " ++ show statements
              , "Rendered statements: " ++ renderedStatements
              , case parsedResult of
                  Right (parsedStatements, "") -> "Parsed statements: " ++ show parsedStatements
                  Right (_, remainingInput)    -> "Remaining input: " ++ remainingInput
                  Left err                     -> "Parse error: " ++ show err
              ]) $
           case parsedResult of
             Right (parsedStatements, remainingInput) ->
               statements == parsedStatements && null remainingInput
             _ -> False
  ]