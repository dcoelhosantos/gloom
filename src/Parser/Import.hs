{-# LANGUAGE RecordWildCards #-}

-- | Module import system for Gloom
--
-- This module implements a simple import system:
--
-- * Parse @import "path/to/file.gloom"@ statements
-- * Load and parse imported files
-- * Detect circular dependencies
-- * Merge imported types and functions into current scope
--
-- Design philosophy: Keep it simple!
-- * Imports are relative to the importing file
-- * No namespace/module system yet (all imports are merged)
-- * Circular imports are detected and rejected
-- * Functions named 'main' in imported files are NOT executed

module Parser.Import
  ( -- * Import Statement Parsing
    parseImportStmt

    -- * Import Processing
  , processImports
  , loadImportedFile
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified Data.Set as Set
import Control.Monad (when, foldM, void)
import Text.Parsec hiding (optional)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified System.IO as IO

import Lexing.Lexer (Token(..), TokenType(..), alexScanTokens, prettyToken)
import qualified Lexing.Tokens.Keywords as Token
import qualified Lexing.Tokens.Literals as Token
import qualified Lexing.Tokens.Symbols as Token
import Parser.General
import qualified Error.Formatter as ErrFmt
import qualified Parser.Types as Type
import Parser.Fun (pFun)


-- | Parse a single import statement: @import "path/to/file.gloom"@
--
-- Returns the file path to import
parseImportStmt :: Interpreter (String, [Token])
parseImportStmt = do
  import_T <- Token.import_
  
  -- Expect string literal with file path
  (path, pathToken) <- Token.textLit
  semiColon_T <- Token.semiColon
  return (path, [import_T, pathToken, semiColon_T])


-- | Process imports: parse all import statements and load imported files
--
-- This function:
-- 1. Parses all @import@ statements at the beginning of the file
-- 2. Resolves relative paths
-- 3. Detects circular dependencies
-- 4. Loads and parses each imported file recursively
-- 5. Merges types and functions into current scope
--
-- Circular import detection uses a Set of visited file paths.
processImports :: FilePath -> Set.Set FilePath -> Interpreter ([Token])
processImports currentFile visitedFiles = do
  -- Parse all import statements
  imports <- many parseImportStmt
  
  if null imports
    then return []
    else do
      let (paths, importTokens) = unzip imports
      
      -- Get directory of current file for relative imports
      let currentDir = Path.takeDirectory currentFile
      
      -- Process each import
      allTokens <- foldM (\accTokens (path, importToken) -> do
        -- Resolve relative path
        let absolutePath = Path.normalise $ currentDir Path.</> path
        
        -- Check for circular dependency
        when (Set.member absolutePath visitedFiles) $ do
          let importCycle = Set.toList visitedFiles ++ [absolutePath]
              line = tokenLine (importToken !! 1)  -- The string literal token
              col = tokenCol (importToken !! 1)
              sourceCode = Text.pack $ "import \"" ++ path ++ "\";"
              errorMsg = ErrFmt.formatCircularImportError 
                           currentFile line col sourceCode 
                           absolutePath importCycle
          liftIO $ TIO.hPutStrLn IO.stderr errorMsg
          fail "Circular import detected"
        
        -- Load and parse imported file
        loadedTokens <- loadImportedFile absolutePath (Set.insert absolutePath visitedFiles)
        
        return $ accTokens ++ loadedTokens
        ) (concat importTokens) (zip paths importTokens)
      
      return allTokens


-- | Load an imported file and parse it
--
-- This function:
-- 1. Reads the file from disk
-- 2. Lexes it into tokens
-- 3. Recursively processes any imports in that file
-- 4. Parses types and functions (but NOT main execution)
-- 5. Merges them into current scope (via RTState side effects)
loadImportedFile :: FilePath -> Set.Set FilePath -> Interpreter [Token]
loadImportedFile filePath visitedFiles = do
  -- Check if file exists
  fileExists <- liftIO $ Dir.doesFileExist filePath
  when (not fileExists) $ do
    let errorMsg = ErrFmt.formatFileNotFoundError 
                     filePath 1 1 
                     (Text.pack $ "import \"" ++ filePath ++ "\";")
                     filePath
    liftIO $ TIO.hPutStrLn IO.stderr errorMsg
    fail "Import file not found"
  
  -- Read file content
  content <- liftIO $ readFile filePath
  
  -- Lex the file
  let fileTokens = alexScanTokens content
  
  -- Set up parser with the imported file's tokens
  oldInput <- getInput
  setInput fileTokens
  
  -- Parse imports recursively first
  void $ processImports filePath visitedFiles
  
  -- Parse top-level declarations (types and functions, but not main)
  -- This modifies RTState directly, adding types and functions to scope
  void $ many parseTopLevelDecl
  
  -- Restore original input
  setInput oldInput
  
  -- Return empty list since we've already modified RTState
  return []
  where
    -- Parse a single top-level declaration (struct or function)
    parseTopLevelDecl :: Interpreter ()
    parseTopLevelDecl = do
      firstToken <- lookAhead anyToken
      case tokenType firstToken of
        Function -> void pFun  -- Parse function
        Struct   -> Type.def   -- Parse struct
        Var      -> fail "Global variables not allowed in imported files"
        Const    -> fail "Global constants not allowed in imported files"
        _        -> fail $ "Unexpected token in imported file: " ++ prettyToken firstToken
