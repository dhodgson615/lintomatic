module Tests.Property.Core where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty, forAll, oneof, listOf1, elements, Property, (==>))
import Data.Char (isSpace)

import Lintomatic.Core

tests :: TestTree
tests = testGroup "Property-Based Tests"
  [ utilityPropertyTests
  , stringPropertyTests
  ]

-- | Property tests for utility functions
utilityPropertyTests :: TestTree
utilityPropertyTests = testGroup "Utility Function Properties"
  [ testProperty "strip is idempotent" prop_stripIdempotent
  , testProperty "strip removes all leading and trailing whitespace" prop_stripRemovesWhitespace
  , testProperty "lstrip only removes leading whitespace" prop_lstripOnlyLeading
  , testProperty "rstrip only removes trailing whitespace" prop_rstripOnlyTrailing
  , testProperty "strip composition property" prop_stripComposition
  ]

-- | Property tests for string processing
stringPropertyTests :: TestTree
stringPropertyTests = testGroup "String Processing Properties"
  [ testProperty "strip preserves non-whitespace content" prop_stripPreservesContent
  , testProperty "empty string handling" prop_emptyStringHandling
  , testProperty "whitespace only strings" prop_whitespaceOnlyStrings
  , testProperty "edge cases" prop_edgeCases
  ]

-- Property: strip is idempotent (applying it twice gives same result as once)
prop_stripIdempotent :: String -> Bool
prop_stripIdempotent s = strip (strip s) == strip s

-- Property: strip removes all leading and trailing whitespace
prop_stripRemovesWhitespace :: String -> Bool
prop_stripRemovesWhitespace s = 
  let result = strip s
  in case result of
    [] -> True
    [x] -> not (isSpace x)
    (x:xs) -> not (isSpace x) && not (isSpace (last xs))

-- Property: lstrip only removes leading whitespace, preserves trailing
prop_lstripOnlyLeading :: String -> Bool
prop_lstripOnlyLeading s = 
  let result = lstrip s
  in case result of
    [] -> True
    (x:_) -> not (isSpace x)

-- Property: rstrip only removes trailing whitespace, preserves leading
prop_rstripOnlyTrailing :: String -> Bool
prop_rstripOnlyTrailing s = 
  let result = rstrip s
  in case reverse result of
    [] -> True
    (x:_) -> not (isSpace x)

-- Property: strip is equivalent to lstrip . rstrip and rstrip . lstrip
prop_stripComposition :: String -> Bool
prop_stripComposition s = 
  strip s == lstrip (rstrip s) && strip s == rstrip (lstrip s)

-- Property: strip preserves the core non-whitespace content
prop_stripPreservesContent :: String -> Bool
prop_stripPreservesContent s = 
  let stripped = strip s
      originalContent = filter (not . isSpace) s
      strippedContent = filter (not . isSpace) stripped
  in originalContent == strippedContent

-- Property: empty string remains empty after any strip operation
prop_emptyStringHandling :: Bool
prop_emptyStringHandling = 
  strip "" == "" && lstrip "" == "" && rstrip "" == ""

-- Property: strings with only whitespace become empty after strip
prop_whitespaceOnlyStrings :: Property
prop_whitespaceOnlyStrings = 
  forAll whitespaceStringGen $ \s -> 
    all isSpace s ==> strip s == ""
  where
    whitespaceStringGen = oneof
      [ return ""
      , listOf1 (elements " \t\n\r\f\v")
      ]

-- Test various edge cases
prop_edgeCases :: Bool
prop_edgeCases = and
  [ strip " " == ""
  , strip "\t\n\r " == ""
  , strip "a" == "a"
  , strip " a " == "a"
  , strip "  a  b  " == "a  b"
  , lstrip " a " == "a "
  , rstrip " a " == " a"
  ]