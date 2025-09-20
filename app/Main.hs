module Main where

import System.Directory (getCurrentDirectory, canonicalizePath, copyFile)
import System.FilePath (makeRelative, takeDirectory)
import Data.List (sort)
import Control.Monad (forM_, when)
import System.Environment (getProgName, getArgs)

import Lintomatic.Core

-- | Configuration for the application
data Config = Config
    { configFix :: Bool        -- ^ Whether to fix issues in-place
    , configBackup :: Bool     -- ^ Whether to backup original files
    , configPath :: FilePath   -- ^ Path to scan (default: current directory)
    } deriving (Show)

-- | Default configuration
defaultConfig :: Config
defaultConfig = Config
    { configFix = False
    , configBackup = False  
    , configPath = "."
    }

-- | Parse command-line arguments into a Config
parseArgs :: [String] -> Config
parseArgs = parseArgs' defaultConfig
  where
    parseArgs' config [] = config
    parseArgs' config ("--fix":rest) = parseArgs' config { configFix = True } rest
    parseArgs' config ("--backup":rest) = parseArgs' config { configBackup = True } rest
    parseArgs' _ ("--help":_) = error helpMessage
    parseArgs' _ ("-h":_) = error helpMessage
    parseArgs' config (path:rest) = parseArgs' config { configPath = path } rest

-- | Help message for the application
helpMessage :: String
helpMessage = unlines
    [ "lintomatic - Python linter and fixer"
    , ""
    , "Usage: lintomatic [OPTIONS] [PATH]"
    , ""
    , "Options:"
    , "  --fix      Fix issues in-place (modifies files)"
    , "  --backup   Create backup files before fixing (use with --fix)"
    , "  --help     Show this help message"
    , ""
    , "Arguments:"
    , "  PATH       Directory to scan (default: current directory)"
    , ""
    , "Examples:"
    , "  lintomatic                    # Check files in current directory"
    , "  lintomatic --fix              # Fix issues in current directory"
    , "  lintomatic --fix --backup     # Fix with backups"
    , "  lintomatic /path/to/project   # Check specific directory"
    ]

{- | Main function that orchestrates the linting process.
     Locates Python files, checks them for docstring length and indentation issues,
     and outputs the results. Can optionally fix issues in-place.
-}
main :: IO ()
main = do
    args <- getArgs
    let config = parseArgs args
    
    execPath <- getProgName
    canonPath <- canonicalizePath execPath
    let repoRoot = takeDirectory canonPath
    cwd <- getCurrentDirectory
    let rootDir = if null repoRoot then cwd else repoRoot
    let scanDir = if configPath config == "." then rootDir else configPath config

    pythonFiles <- findPythonFiles scanDir

    if configFix config
        then runFixMode config scanDir pythonFiles
        else runCheckMode scanDir pythonFiles

-- | Run in check-only mode (original behavior)
runCheckMode :: FilePath -> [FilePath] -> IO ()
runCheckMode rootDir pythonFiles = do
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

-- | Run in fix mode (new functionality)
runFixMode :: Config -> FilePath -> [FilePath] -> IO ()
runFixMode config rootDir pythonFiles = do
    putStrLn $ "Running in fix mode" ++ (if configBackup config then " with backups" else "")
    
    forM_ pythonFiles $ \filePath -> do
        let relativePath = makeRelative rootDir filePath
        
        -- Create backup if requested
        when (configBackup config) $ do
            let backupPath = filePath ++ ".bak"
            copyFile filePath backupPath
            putStrLn $ "Created backup: " ++ makeRelative rootDir backupPath
        
        -- Read the file content
        content <- readFile filePath
        
        -- Apply fixes
        fixedContent <- applyFixes filePath content
        
        -- Write back if changes were made
        if content /= fixedContent
            then do
                writeFile filePath fixedContent
                putStrLn $ "Fixed: " ++ relativePath
            else
                putStrLn $ "No fixes needed: " ++ relativePath