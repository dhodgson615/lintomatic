module Main where

import System.Directory (getCurrentDirectory, canonicalizePath)
import System.FilePath (makeRelative, takeDirectory)
import Data.List (sort)
import Control.Monad (forM_, when)
import System.Environment (getProgName)

import Lintomatic.Core

{- | Main function that orchestrates the linting process.
     Locates Python files, checks them for docstring length and indentation issues,
     and outputs the results.
-}
main :: IO ()
main = do
    execPath <- getProgName
    canonPath <- canonicalizePath execPath
    let repoRoot = takeDirectory canonPath
    cwd <- getCurrentDirectory
    let rootDir = if null repoRoot then cwd else repoRoot

    pythonFiles <- findPythonFiles rootDir

    forM_ pythonFiles $ \filePath -> do
        let relativePath = makeRelative rootDir filePath
        longDocstrings <- checkDocstringLength filePath 72
        problematicIndents <- checkIndentation filePath
        blockStatementIssues <- checkBlockStatements filePath
        keywordStatementIssues <- checkKeywordStatements filePath

        when (not (null longDocstrings) || not (null problematicIndents) || not (null blockStatementIssues) || not (null keywordStatementIssues)) $ do
            putStrLn $ "\nFile: " ++ relativePath

            when (not $ null longDocstrings) $ do
                putStrLn "        Docstring lines exceeding 72 characters:"
                forM_ (sort longDocstrings) $ \lineNum ->
                    putStrLn $ "            Line " ++ show lineNum

            when (not $ null problematicIndents) $ do
                putStrLn "        Lines with problematic indentation:"
                forM_ (sort problematicIndents) $ \lineNum ->
                    putStrLn $ "            Line " ++ show lineNum

            when (not $ null blockStatementIssues) $ do
                putStrLn "        Block statements missing blank lines above:"
                forM_ (sort blockStatementIssues) $ \lineNum ->
                    putStrLn $ "            Line " ++ show lineNum

            when (not $ null keywordStatementIssues) $ do
                putStrLn "        Keyword statements missing blank lines from non-keyword statements:"
                forM_ (sort keywordStatementIssues) $ \lineNum ->
                    putStrLn $ "            Line " ++ show lineNum