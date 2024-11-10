{-# LANGUAGE ImportQualifiedPost #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib1 qualified
import Lib2 qualified
import Lib2 (Query(..), Operand(..), Nucleotide(..), State(..), emptyState)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
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
    [ 
    testCase "Missing operand in concat" $
        case Lib2.parseQuery "concat AT" of
          Left _ -> return () -- Expected error
          Right _ -> error "Test failed: Should have rejected incomplete concat"
          
    , testCase "Delete non-existent sequence" $
        case Lib2.stateTransition emptyState (DeleteSeq "nonexistent") of
          Left _ -> return () -- Expected error
          Right _ -> error "Test failed: Should have rejected deleting non-existent sequence"
    ]
  ]