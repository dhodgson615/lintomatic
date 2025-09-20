module Main where

import System.Directory (doesDirectoryExist, listDirectory, getCurrentDirectory, canonicalizePath)
import System.FilePath ((</>), takeExtension, makeRelative, takeDirectory)
import Data.List (isPrefixOf, isSuffixOf, sort, isInfixOf)
import Control.Monad (filterM, forM_, when)
import System.Environment (getProgName)
import Data.Char (isSpace)

{- | Find all Python files in a directory recursively.
     Traverses the directory tree and returns a list of paths to all .py files.

     Parameters:
     - rootDir: The directory to start searching from

     Returns: A list of full paths to all Python files found
-}
findPythonFiles :: FilePath -> IO [FilePath]
findPythonFiles rootDir = do
    isDir <- doesDirectoryExist rootDir
    if isDir
        then do
            entries <- listDirectory rootDir
            let fullPaths = map (rootDir </>) entries
            dirs <- filterM doesDirectoryExist fullPaths
            let files = filter (\f -> takeExtension f == ".py") fullPaths
            subDirFiles <- concat <$> mapM findPythonFiles dirs
            return $ files ++ subDirFiles
        else return []

{- | Find docstring lines that exceed the maximum length.
     Reads a Python file and identifies lines within docstrings that are longer
     than the specified character limit.

     Parameters:
     - filePath: Path to the Python file to check
     - maxLength: Maximum allowed line length (default is 72)

     Returns: A list of line numbers where docstring lines exceed the maximum length
-}
checkDocstringLength :: FilePath -> Int -> IO [Int]
checkDocstringLength filePath maxLength = do
    content <- readFile filePath
    let lines' = lines content
    return $ findLongLines lines' 1 False Nothing []
  where
    findLongLines [] _ _ _ acc = reverse acc
    findLongLines (line:rest) lineNum inDocstring delimiter acc =
        let stripped = strip line
            newLineNum = lineNum + 1
        in
        if null stripped
            then findLongLines rest newLineNum inDocstring delimiter acc
            else if not inDocstring && (isPrefixOf "\"\"\"" stripped || isPrefixOf "'''" stripped)
                then
                    let newDelimiter = if isPrefixOf "\"\"\"" stripped then "\"\"\"" else "'''"
                        endsOnSameLine = isSuffixOf newDelimiter stripped && length stripped > 6
                        newInDocstring = not endsOnSameLine
                        newAcc = if length line > maxLength then lineNum : acc else acc
                    in findLongLines rest newLineNum newInDocstring (Just newDelimiter) newAcc
                else if inDocstring
                    then
                        let hasDelimiter = maybe False (`isInfixOf` line) delimiter
                            newInDocstring = not hasDelimiter
                            newAcc = if length line > maxLength then lineNum : acc else acc
                        in findLongLines rest newLineNum newInDocstring delimiter newAcc
                    else findLongLines rest newLineNum inDocstring delimiter acc

{- | Find block statements that lack blank lines before them.
     Identifies Python block statements (if, for, while, etc.) that should have
     blank lines before them for better readability.

     Parameters:
     - filePath: Path to the Python file to check

     Returns: A list of line numbers where block statements need blank lines before them
-}
checkBlockStatements :: FilePath -> IO [Int]
checkBlockStatements filePath = do
    content <- readFile filePath
    let lines' = lines content
    return $ findBlockViolations lines' 1 []
  where
    findBlockViolations [] _ acc = reverse acc
    findBlockViolations [_] _ acc = reverse acc
    findBlockViolations (prev:curr:rest) lineNum acc =
        let strippedPrev = strip prev
            strippedCurr = strip curr
            currIndent = length curr - length (lstrip curr)
            prevIndent = length prev - length (lstrip prev)
            nextLineNum = lineNum + 1
            isBlockStatement = any (`isPrefixOf` strippedCurr) blockKeywords
            -- Only flag if it's a block statement, has content before it, and isn't directly nested
            needsBlankLine = isBlockStatement && 
                           not (null strippedPrev) && 
                           not (null strippedCurr) &&
                           currIndent <= prevIndent  -- Only flag if not indented deeper
            newAcc = if needsBlankLine then nextLineNum : acc else acc
        in findBlockViolations (curr:rest) nextLineNum newAcc
    
    blockKeywords = ["if ", "elif ", "else:", "for ", "while ", "try:", "except ", "finally:", "with ", "def ", "class "]

{- | Find lines where indentation decreases without a blank line above.
     Identifies style issues where code dedentation occurs without proper separation.

     Parameters:
     - filePath: Path to the Python file to check

     Returns: A list of line numbers where problematic indentation is found
-}
checkIndentation :: FilePath -> IO [Int]
checkIndentation filePath = do
    content <- readFile filePath
    let lines' = lines content
    return $ findProblematicLines lines' 1 []
  where
    findProblematicLines [] _ acc = reverse acc
    findProblematicLines [_] _ acc = reverse acc
    findProblematicLines (prev:curr:rest) lineNum acc =
        let strippedPrev = strip prev
            strippedCurr = strip curr
            prevIndent = length prev - length (lstrip prev)
            currIndent = length curr - length (lstrip curr)
            nextLineNum = lineNum + 1
        in
        if null strippedPrev || null strippedCurr
            then findProblematicLines (curr:rest) nextLineNum acc
            else if currIndent < prevIndent
                then
                    let isExempt = case strippedCurr of
                                     (c:_) -> elem c "(){}[]"
                                     [] -> False
                        newAcc = if isExempt then acc else nextLineNum : acc
                    in findProblematicLines (curr:rest) nextLineNum newAcc
                else findProblematicLines (curr:rest) nextLineNum acc

{- | Find keyword statements that lack blank lines from non-keyword statements.
     Identifies cases where keyword statements follow non-keyword statements
     (or vice versa) without proper blank line separation for readability.

     Parameters:
     - filePath: Path to the Python file to check

     Returns: A list of line numbers where keyword statements need blank lines
-}
checkKeywordStatements :: FilePath -> IO [Int]
checkKeywordStatements filePath = do
    content <- readFile filePath
    let lines' = lines content
    return $ findKeywordViolations lines' 1 []
  where
    findKeywordViolations [] _ acc = reverse acc
    findKeywordViolations [_] _ acc = reverse acc
    findKeywordViolations (prev:curr:rest) lineNum acc =
        let strippedPrev = strip prev
            strippedCurr = strip curr
            currIndent = length curr - length (lstrip curr)
            prevIndent = length prev - length (lstrip prev)
            nextLineNum = lineNum + 1
            isKeywordPrev = isKeywordStatement strippedPrev
            isKeywordCurr = isKeywordStatement strippedCurr
            -- Only flag if both lines have content, are at same indent level,
            -- and transition between keyword and non-keyword statements
            -- But exclude some common cases like docstrings and function bodies
            isDocstringPrev = "\"\"\"" `isInfixOf` strippedPrev || "'''" `isInfixOf` strippedPrev
            needsBlankLine = not (null strippedPrev) && 
                           not (null strippedCurr) &&
                           currIndent == prevIndent &&
                           currIndent == 0 &&  -- Only flag at module level
                           not isDocstringPrev &&  -- Don't flag after docstrings
                           isKeywordPrev /= isKeywordCurr
            newAcc = if needsBlankLine then nextLineNum : acc else acc
        in findKeywordViolations (curr:rest) nextLineNum newAcc
    
    isKeywordStatement line = any (`isPrefixOf` line) keywordPrefixes
    
    keywordPrefixes = [
        "assert ", "return", "if ", "elif ", "else:", "for ", "while ", 
        "try:", "except ", "finally:", "with ", "def ", "class ", 
        "import ", "from ", "break", "continue", "pass", "raise ", 
        "yield ", "global ", "nonlocal "
        ]

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

{- | Remove whitespace from both ends of a string.
-}
strip :: String -> String
strip = lstrip . rstrip

{- | Remove leading whitespace from a string.
-}
lstrip :: String -> String
lstrip = dropWhile isSpace

{- | Remove trailing whitespace from a string.
-}
rstrip :: String -> String
rstrip = reverse . lstrip . reverse
