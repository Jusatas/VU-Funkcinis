{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ 
    testCase "Parse valid nucleotide sequence" $
      Lib2.parseNucSeq "ATGCG" @?= Right ("ATGCG", ""),
      
    testCase "Parse invalid nucleotide sequence" $
      Lib2.parseNucSeq "ATB" @?= Left "Error: invalid nucleotide.",
      
    testCase "Parse concat query" $
      Lib2.parseQuery "concat ATG CGT" @?= Right (Lib2.Concat "ATG" "CGT"),
      
    testCase "Parse complement query" $
      Lib2.parseQuery "complement ATG" @?= Right (Lib2.Complement "ATG"),
      
    testCase "Parse transcribe query" $
      Lib2.parseQuery "transcribe ATG" @?= Right (Lib2.Transcribe "ATG"),
      
    testCase "Parse mutate query" $
      Lib2.parseQuery "mutate ATG 50" @?= Right (Lib2.Mutate "ATG" 50),
      
    testCase "Parse invalid command" $
      Lib2.parseQuery "invalid ATG" @?= Left "Error: Unknown command.",
      
    testCase "State transition - concat sequences" $
      let state = Lib2.emptyState
          query = Lib2.Concat "ATG" "CGT"
      in Lib2.stateTransition state query @?= Right (Just "Concatenated: ATGCGT", Lib2.State "ATGCGT"),
      
    testCase "State transition - complement sequence" $
      let state = Lib2.emptyState
          query = Lib2.Complement "ATG"
      in Lib2.stateTransition state query @?= Right (Just "Complement: TAC", Lib2.State "TAC"),
      
    testCase "State transition - transcribe sequence" $
      let state = Lib2.emptyState
          query = Lib2.Transcribe "ATG"
      in Lib2.stateTransition state query @?= Right (Just "Transcribed: AUG", Lib2.State "AUG"),
      
    testCase "Parse with parentheses - nested operations" $
      Lib2.parseQuery "mutate (concat ATG (complement G)) 50" @?= Right "ATGC"
  ]
