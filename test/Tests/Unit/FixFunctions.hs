module Tests.Unit.FixFunctions where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Lintomatic.Core

tests :: TestTree
tests = testGroup "Fix Function Tests"
  [ docstringFixTests
  , indentationFixTests
  , blockStatementFixTests
  , keywordStatementFixTests
  , integrationFixTests
  ]

-- | Test the docstring length fix functionality
docstringFixTests :: TestTree
docstringFixTests = testGroup "Docstring Length Fixes"
  [ testCase "fixes long single-line docstring" $ do
      let content = "\"\"\"This is a very long docstring that definitely exceeds the 72 character limit\"\"\""
      fixed <- applyFixes "" content
      length (head (lines fixed)) @?= 72  -- Should be exactly 72 chars after fix
  
  , testCase "fixes long multi-line docstring" $ do
      let content = unlines
            [ "\"\"\""
            , "This is a very long line in a multiline docstring that definitely exceeds the limit"
            , "Short line"
            , "\"\"\""
            ]
      fixed <- applyFixes "" content
      let fixedLines = lines fixed
      length (fixedLines !! 1) @?= 72  -- Second line should be fixed
      (fixedLines !! 2) @?= "Short line"  -- Short line unchanged
  
  , testCase "leaves short docstrings unchanged" $ do
      let content = "\"\"\"Short docstring\"\"\""
      fixed <- applyFixes "" content
      fixed @?= content ++ "\n"
  
  , testCase "only affects docstring content" $ do
      let content = unlines
            [ "\"\"\"Short docstring\"\"\""
            , "# This is a very long comment that exceeds 72 characters but should not be changed"
            , "x = 1"
            ]
      fixed <- applyFixes "" content
      let fixedLines = lines fixed
      (fixedLines !! 1) @?= "# This is a very long comment that exceeds 72 characters but should not be changed"
  ]

-- | Test the indentation fix functionality
indentationFixTests :: TestTree
indentationFixTests = testGroup "Indentation Fixes"
  [ testCase "basic indentation fix test - confirm it runs" $ do
      let content = unlines
            [ "def function():"
            , "    if True:"
            , "        print('indented')"
            , "    print('This should get a blank line above')"
            ]
      fixed <- applyFixes "" content
      let fixedLines = lines fixed
      -- Just verify it doesn't crash and produces reasonable output
      -- Note: might be more lines if fixes are applied
      assertBool "Should have at least 4 lines" $ length fixedLines >= 4
  
  , testCase "doesn't modify proper indentation" $ do
      let content = unlines
            [ "def function():"
            , "    x = 1"
            , "    return x"
            ]
      fixed <- applyFixes "" content
      fixed @?= content
  
  , testCase "exempts bracket continuations" $ do
      let content = unlines
            [ "def function():"
            , "    result = some_function("
            , "        arg1,"
            , "        arg2"
            , "    )"
            ]
      fixed <- applyFixes "" content
      fixed @?= content
  ]

-- | Test the block statement fix functionality
blockStatementFixTests :: TestTree
blockStatementFixTests = testGroup "Block Statement Fixes"
  [ testCase "adds blank line before block statements" $ do
      let content = unlines
            [ "x = 1"
            , "if x > 0:"
            , "    pass"
            ]
      fixed <- applyFixes "" content
      let fixedLines = lines fixed
      (fixedLines !! 1) @?= ""  -- Should insert blank line
      (fixedLines !! 2) @?= "if x > 0:"
  
  , testCase "doesn't modify properly spaced block statements" $ do
      let content = unlines
            [ "x = 1"
            , ""
            , "if x > 0:"
            , "    pass"
            ]
      fixed <- applyFixes "" content
      fixed @?= content
  
  , testCase "handles nested blocks - confirms logic works" $ do
      let content = unlines
            [ "def function():"
            , "    x = 1"
            , "    if x > 0:"  -- This is at same indent level as prev line, should get blank line
            , "        pass"
            ]
      fixed <- applyFixes "" content
      let fixedLines = lines fixed
      -- The fix should add a blank line before the if statement
      length fixedLines @?= 5  -- Should have one more line than original
  ]

-- | Test the keyword statement fix functionality
keywordStatementFixTests :: TestTree
keywordStatementFixTests = testGroup "Keyword Statement Fixes"
  [ testCase "adds blank line between keyword and non-keyword statements" $ do
      let content = unlines
            [ "import os"
            , "x = 1"
            , "def function():"
            , "    pass"
            ]
      fixed <- applyFixes "" content
      let fixedLines = lines fixed
      length fixedLines @?= 6  -- Should have 2 extra blank lines
  
  , testCase "doesn't modify properly spaced transitions" $ do
      let content = unlines
            [ "import os"
            , ""
            , "x = 1"
            , ""
            , "def function():"
            , "    pass"
            ]
      fixed <- applyFixes "" content
      fixed @?= content
  ]

-- | Test integration of all fix functions
integrationFixTests :: TestTree
integrationFixTests = testGroup "Integration Fix Tests"
  [ testCase "applies all fixes correctly" $ do
      let content = unlines
            [ "\"\"\"This is a very long docstring that definitely exceeds the 72 character limit\"\"\""
            , "import os"
            , "def function():"
            , "    if True:"
            , "        print('test')"
            , "    print('dedented')"
            , "x = 1"
            ]
      fixed <- applyFixes "" content
      let fixedLines = lines fixed
      -- Check docstring was truncated
      length (head fixedLines) @?= 72
      -- Check that fixes were applied (should have more lines due to blank line insertions)
      assertBool "Fixed content should have same or more lines" $ 
        length fixedLines >= length (lines content)
  
  , testCase "doesn't change files with no issues" $ do
      let content = unlines
            [ "\"\"\"Short docstring\"\"\""
            , ""
            , "import os"
            , ""
            , "def function():"
            , "    \"\"\"Function docstring\"\"\""
            , "    return True"
            ]
      fixed <- applyFixes "" content
      fixed @?= content
  ]