{-|
Module      : Analysis
Description : Static analysis interface for LSP integration
Copyright   : (c) João, 2024-2025
License     : MIT
Stability   : experimental

This module provides a clean interface for running static analysis
on Gloom source code, suitable for use by both the interpreter and LSP.
-}
module Analysis
  ( analyzeSource
  , AnalysisResult(..)
  , AnalysisError(..)
  ) where

import Text.Parsec (runParserT, ParseError, errorPos, getState)
import Text.Parsec.Error (errorMessages, messageString)
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)

-- Local imports
import Parser.TopLevel (program)
import Lexing.Lexer (tokenize)
import RTState (freshState, RTState, checkUnresolvedDebits)

-- | Result of static analysis
data AnalysisResult
  = AnalysisSuccess RTState
  | AnalysisFailure [AnalysisError]
  deriving (Show)

-- | Structured error information
data AnalysisError = AnalysisError
  { errFile    :: FilePath
  , errLine    :: Int
  , errColumn  :: Int
  , errMessage :: String
  } deriving (Show, Eq)

-- | Analyze Gloom source code from a string
--
-- This runs Phase 1 (Analysing) without executing the program.
-- Returns either a list of errors or the final state after analysis.
--
-- Example:
-- @
-- result <- analyzeSource "test.gloom" "fn main() { print(\"hello\"); }"
-- case result of
--   AnalysisSuccess state -> putStrLn "✓ No errors"
--   AnalysisFailure errs  -> mapM_ print errs
-- @
analyzeSource :: FilePath -> String -> IO AnalysisResult
analyzeSource filepath sourceCode = do
  -- Tokenize the source
  let tokens = tokenize filepath sourceCode
  
  -- Run Phase 1 (Analysing) - type checking and validation
  let analysingState = freshState
  result <- runParserT (program >> getState) analysingState filepath tokens
  
  case result of
    Left parseErr -> do
      -- Convert ParseError to structured format
      let err = parseErrorToAnalysisError parseErr
      pure $ AnalysisFailure [err]
    
    Right finalState -> do
      -- Check for unresolved function calls (mutual recursion issues)
      case checkUnresolvedDebits finalState of
        Just debitError -> do
          -- Convert debit error to structured format
          let err = AnalysisError
                { errFile = filepath
                , errLine = 0  -- Debit errors don't have specific line
                , errColumn = 0
                , errMessage = debitError
                }
          pure $ AnalysisFailure [err]
        
        Nothing ->
          pure $ AnalysisSuccess finalState

-- | Convert Parsec's ParseError to our structured format
parseErrorToAnalysisError :: ParseError -> AnalysisError
parseErrorToAnalysisError parseErr =
  let pos = errorPos parseErr
      msgs = errorMessages parseErr
      msgStrs = map messageString msgs
      combinedMsg = unwords msgStrs
  in AnalysisError
    { errFile    = sourceName pos
    , errLine    = sourceLine pos
    , errColumn  = sourceColumn pos
    , errMessage = if null combinedMsg then "Parse error" else combinedMsg
    }
