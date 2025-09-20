module Tests.Unit.FileDiscovery where

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectory)
import Data.List (sort)

import Lintomatic.Core

tests :: TestTree
tests = testGroup "File Discovery Tests"
  [ testCase "finds Python files in simple directory" $ do
      withTempFileStructure simpleStructure $ \tmpDir -> do
        files <- findPythonFiles tmpDir
        let relativeFiles = map (makeRelativeTo tmpDir) (sort files)
        relativeFiles @?= ["file1.py", "file2.py"]
  
  , testCase "finds Python files recursively" $ do
      withTempFileStructure nestedStructure $ \tmpDir -> do
        files <- findPythonFiles tmpDir
        let relativeFiles = map (makeRelativeTo tmpDir) (sort files)
        relativeFiles @?= ["file1.py", "subdir/file3.py", "subdir/nested/file4.py"]
  
  , testCase "ignores non-Python files" $ do
      withTempFileStructure mixedFilesStructure $ \tmpDir -> do
        files <- findPythonFiles tmpDir
        let relativeFiles = map (makeRelativeTo tmpDir) (sort files)
        relativeFiles @?= ["script.py"]
  
  , testCase "handles empty directory" $ do
      withSystemTempDirectory "lintomatic-test" $ \tmpDir -> do
        files <- findPythonFiles tmpDir
        files @?= []
  
  , testCase "handles nonexistent directory" $ do
      files <- findPythonFiles "/nonexistent/directory"
      files @?= []
  
  , testCase "handles deeply nested directory structure" $ do
      withTempFileStructure deeplyNestedStructure $ \tmpDir -> do
        files <- findPythonFiles tmpDir
        let relativeFiles = map (makeRelativeTo tmpDir) (sort files)
        relativeFiles @?= ["deep/very/deeply/nested/deep.py", "top.py"]
  
  , testCase "handles directory with special characters in filenames" $ do
      withTempFileStructure specialCharStructure $ \tmpDir -> do
        files <- findPythonFiles tmpDir
        let relativeFiles = map (makeRelativeTo tmpDir) (sort files)
        relativeFiles @?= ["normal.py", "with-dash.py", "with_underscore.py"]
  ]

-- Helper types and functions for creating test file structures
data FileStructure = File String String | Dir String [FileStructure]

simpleStructure :: [FileStructure]
simpleStructure = 
  [ File "file1.py" "# Python file 1"
  , File "file2.py" "# Python file 2"
  , File "readme.txt" "Not a Python file"
  ]

nestedStructure :: [FileStructure]
nestedStructure = 
  [ File "file1.py" "# Root Python file"
  , Dir "subdir" 
    [ File "file3.py" "# Nested Python file"
    , Dir "nested"
      [ File "file4.py" "# Deeply nested Python file"
      ]
    ]
  , File "other.txt" "Not Python"
  ]

mixedFilesStructure :: [FileStructure]
mixedFilesStructure = 
  [ File "script.py" "#!/usr/bin/env python"
  , File "config.json" "{}"
  , File "readme.md" "# README"
  , File "Makefile" "all:"
  ]

deeplyNestedStructure :: [FileStructure]
deeplyNestedStructure = 
  [ File "top.py" "# Top level Python file"
  , Dir "deep"
    [ Dir "very"
      [ Dir "deeply"
        [ Dir "nested"
          [ File "deep.py" "# Deeply nested Python file"
          ]
        ]
      ]
    ]
  ]

specialCharStructure :: [FileStructure]
specialCharStructure = 
  [ File "normal.py" "# Normal filename"
  , File "with-dash.py" "# Filename with dash"
  , File "with_underscore.py" "# Filename with underscore"
  , File "not-python.txt" "Not a Python file"
  ]

-- Create a temporary directory with the specified file structure
withTempFileStructure :: [FileStructure] -> (FilePath -> IO a) -> IO a
withTempFileStructure structure action =
  withSystemTempDirectory "lintomatic-test" $ \tmpDir -> do
    createFileStructure tmpDir structure
    action tmpDir

createFileStructure :: FilePath -> [FileStructure] -> IO ()
createFileStructure basePath structure = mapM_ (createItem basePath) structure
  where
    createItem :: FilePath -> FileStructure -> IO ()
    createItem dir (File name content) = writeFile (dir </> name) content
    createItem dir (Dir name items) = do
      let subdir = dir </> name
      createDirectory subdir
      createFileStructure subdir items

-- Helper to make paths relative for easier testing
makeRelativeTo :: FilePath -> FilePath -> String
makeRelativeTo base path = 
  if base `isPrefixOf` path
    then drop (length base + 1) path  -- +1 for the path separator
    else path
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys