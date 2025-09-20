module Tests.Unit.Core where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Lintomatic.Core

tests :: TestTree
tests = testGroup "Core Function Tests"
  [ utilityFunctionTests
  , docstringLengthTests
  , indentationTests
  , blockStatementTests
  , keywordStatementTests
  , edgeCaseTests
  ]

-- | Test the utility string manipulation functions
utilityFunctionTests :: TestTree
utilityFunctionTests = testGroup "Utility Functions"
  [ testCase "strip removes leading and trailing whitespace" $
      strip "  hello world  " @?= "hello world"
  
  , testCase "strip handles empty string" $
      strip "" @?= ""
  
  , testCase "strip handles string with only whitespace" $
      strip "   " @?= ""
  
  , testCase "lstrip removes only leading whitespace" $
      lstrip "  hello world  " @?= "hello world  "
  
  , testCase "rstrip removes only trailing whitespace" $
      rstrip "  hello world  " @?= "  hello world"
  
  , testCase "strip handles newlines and tabs" $
      strip "\t\n  hello world  \n\t" @?= "hello world"
  ]

-- | Test docstring length checking functionality
docstringLengthTests :: TestTree
docstringLengthTests = testGroup "Docstring Length Tests"
  [ testCase "detects long single-line docstring" $ do
      withTempPythonFile longSingleLineDocstring $ \path -> do
        violations <- checkDocstringLength path 72
        violations @?= [1]
  
  , testCase "detects long multi-line docstring lines" $ do
      withTempPythonFile longMultiLineDocstring $ \path -> do
        violations <- checkDocstringLength path 72
        violations @?= [2, 4]
  
  , testCase "ignores short docstrings" $ do
      withTempPythonFile shortDocstring $ \path -> do
        violations <- checkDocstringLength path 72
        violations @?= []
  
  , testCase "handles triple single quotes" $ do
      withTempPythonFile tripleSingleQuoteDocstring $ \path -> do
        violations <- checkDocstringLength path 72
        violations @?= [2]
  
  , testCase "handles file without docstrings" $ do
      withTempPythonFile noDocstring $ \path -> do
        violations <- checkDocstringLength path 72
        violations @?= []
  ]

-- | Test indentation checking functionality
indentationTests :: TestTree
indentationTests = testGroup "Indentation Tests"
  [ testCase "detects problematic dedentation" $ do
      withTempPythonFile problematicIndentation $ \path -> do
        violations <- checkIndentation path
        violations @?= [4]
  
  , testCase "ignores proper indentation" $ do
      withTempPythonFile properIndentation $ \path -> do
        violations <- checkIndentation path
        violations @?= []
  
  , testCase "exempts bracket continuations" $ do
      withTempPythonFile bracketContinuation $ \path -> do
        violations <- checkIndentation path
        violations @?= []
  ]

-- | Test block statement checking functionality
blockStatementTests :: TestTree
blockStatementTests = testGroup "Block Statement Tests"
  [ testCase "detects block statements without blank lines" $ do
      withTempPythonFile blockWithoutBlankLine $ \path -> do
        violations <- checkBlockStatements path
        violations @?= [3]
  
  , testCase "ignores properly spaced block statements" $ do
      withTempPythonFile blockWithBlankLine $ \path -> do
        violations <- checkBlockStatements path
        violations @?= []
  ]

-- | Test keyword statement checking functionality
keywordStatementTests :: TestTree
keywordStatementTests = testGroup "Keyword Statement Tests"
  [ testCase "detects keyword/non-keyword transitions without blank lines" $ do
      withTempPythonFile keywordTransition $ \path -> do
        violations <- checkKeywordStatements path
        violations @?= [2, 3]
  
  , testCase "ignores properly spaced keyword transitions" $ do
      withTempPythonFile properKeywordSpacing $ \path -> do
        violations <- checkKeywordStatements path
        violations @?= []
  ]

-- Helper function to create temporary Python files for testing
withTempPythonFile :: String -> (FilePath -> IO a) -> IO a
withTempPythonFile content action = 
  withSystemTempDirectory "lintomatic-test" $ \tmpDir -> do
    let filepath = tmpDir </> "test.py"
    writeFile filepath content
    action filepath

-- Test data
longSingleLineDocstring :: String
longSingleLineDocstring = "\"\"\"This is a very long docstring that definitely exceeds the 72 character limit\"\"\""

longMultiLineDocstring :: String
longMultiLineDocstring = unlines
  [ "\"\"\""
  , "This is a very long line in a multiline docstring that definitely exceeds the limit"
  , "Short line"
  , "Another very long line in the docstring that should definitely be flagged as well"
  , "\"\"\""
  ]

shortDocstring :: String
shortDocstring = "\"\"\"Short docstring\"\"\""

tripleSingleQuoteDocstring :: String
tripleSingleQuoteDocstring = unlines
  [ "'''"
  , "This is a very long line using single quotes that definitely exceeds the limit"
  , "'''"
  ]

noDocstring :: String
noDocstring = unlines
  [ "def function():"
  , "    return 42"
  ]

problematicIndentation :: String
problematicIndentation = unlines
  [ "def function():"
  , "    if True:"
  , "        print('indented')"
  , "    print('problematic dedent')"
  ]

properIndentation :: String
properIndentation = unlines
  [ "def function():"
  , "    if True:"
  , "        print('indented')"
  , ""
  , "    print('proper dedent')"
  ]

bracketContinuation :: String
bracketContinuation = unlines
  [ "def function():"
  , "    result = some_call("
  , "        argument1,"
  , "    )"
  , "    return result"
  ]

blockWithoutBlankLine :: String
blockWithoutBlankLine = unlines
  [ "x = 1"
  , "y = 2"
  , "if x > y:"
  , "    print('x is greater')"
  ]

blockWithBlankLine :: String
blockWithBlankLine = unlines
  [ "x = 1"
  , "y = 2"
  , ""
  , "if x > y:"
  , "    print('x is greater')"
  ]

keywordTransition :: String
keywordTransition = unlines
  [ "import os"
  , "x = 1"
  , "import sys"
  ]

properKeywordSpacing :: String
properKeywordSpacing = unlines
  [ "import os"
  , "import sys"
  , ""
  , "x = 1"
  ]

-- | Test comprehensive edge cases
edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Case Tests"
  [ testCase "handles empty Python file" $ do
      withTempPythonFile "" $ \path -> do
        docViolations <- checkDocstringLength path 72
        indentViolations <- checkIndentation path
        blockViolations <- checkBlockStatements path
        keywordViolations <- checkKeywordStatements path
        docViolations @?= []
        indentViolations @?= []
        blockViolations @?= []
        keywordViolations @?= []
  
  , testCase "handles file with only whitespace" $ do
      withTempPythonFile "   \n\t\n   " $ \path -> do
        docViolations <- checkDocstringLength path 72
        indentViolations <- checkIndentation path
        blockViolations <- checkBlockStatements path
        keywordViolations <- checkKeywordStatements path
        docViolations @?= []
        indentViolations @?= []
        blockViolations @?= []
        keywordViolations @?= []
  
  , testCase "handles extremely long docstring" $ do
      withTempPythonFile extremeLongDocstring $ \path -> do
        violations <- checkDocstringLength path 72
        violations @?= [1]
  
  , testCase "handles complex nested indentation" $ do
      withTempPythonFile complexNestedIndentation $ \path -> do
        violations <- checkIndentation path
        violations @?= [6]
  
  , testCase "handles mixed quote docstrings" $ do
      withTempPythonFile mixedQuoteDocstrings $ \path -> do
        violations <- checkDocstringLength path 72
        violations @?= [5]
  
  , testCase "handles very short line length limit" $ do
      withTempPythonFile shortDocstring $ \path -> do
        violations <- checkDocstringLength path 10
        violations @?= [1]
  
  , testCase "handles file with only comments" $ do
      withTempPythonFile onlyComments $ \path -> do
        docViolations <- checkDocstringLength path 72
        indentViolations <- checkIndentation path
        blockViolations <- checkBlockStatements path
        keywordViolations <- checkKeywordStatements path
        docViolations @?= []
        indentViolations @?= []
        blockViolations @?= []
        keywordViolations @?= []
  ]

-- Test data for edge cases
extremeLongDocstring :: String
extremeLongDocstring = "\"\"\"This is an extremely long docstring that goes way beyond any reasonable character limit and should definitely be flagged\"\"\""

complexNestedIndentation :: String
complexNestedIndentation = unlines
  [ "def function():"
  , "    for i in range(10):"
  , "        if i > 5:"
  , "            print(i)"
  , "            print('nested')"
  , "    print('problematic dedent')"
  , "        print('should not happen')"
  ]

mixedQuoteDocstrings :: String
mixedQuoteDocstrings = unlines
  [ "\"\"\""
  , "This is a very long line that exceeds the limit and uses double quotes"
  , "\"\"\""
  , "'''"
  , "This is another very long line that exceeds the limit but uses single quotes"
  , "'''"
  ]

onlyComments :: String
onlyComments = unlines
  [ "# This is a comment"
  , "# This is another comment that might be very long but should not be flagged"
  , "# More comments here"
  ]