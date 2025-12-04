{-|
Module      : Parser.File
Description : Testing utilities and REPL-style parsers for Gloom development
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module provides testing and interactive development utilities for the Gloom parser.
It's primarily used in GHCi for quick testing and experimentation.

= Main Testing Function

The 'test' function runs the complete two-phase execution model:

1. __Phase 1__ (Analysing): Type checking and symbol collection
2. __Phase 2__ (Running): Actual execution

= Interactive Parsers

Additional functions ('typε', 'expr', 'stmts') allow testing individual
grammar components in isolation.

= Usage in GHCi

@
ghci> :load Parser.File
ghci> test "code-examples/01-basics/hello-world.gloom"
ghci> expr "2 + 3 * 4"
@
-}
module Parser.File
  ( -- * Main Testing Function
    test

    -- * Interactive Parsers
  , typε
  , expr
  , stmts
  ) where


-- global imports
import Text.Parsec (runParserT, getState)
import Text.Parsec hiding (tokens, token, runParserT, getState)

-- local imports
import qualified Parser.Types as Type
import Parser.TopLevel
import qualified Parser.Fun as Parser
import Lexing.Lexer
import RTState
-- import qualified Parser.TopLevel as Top

-- | Run a complete two-phase test of a Gloom program
--
-- This mirrors the execution model in "Main", useful for interactive testing in GHCi.
--
-- = Phase 1: Static Analysis
--
-- Type checks the program with placeholder values, collecting:
--
-- * Global variables and their types
-- * Function signatures
-- * User-defined types (structs, variants)
-- * Const initialization validation
--
-- = Phase 2: Execution
--
-- Runs the program with actual runtime values, using the state from phase 1.
--
-- == Example Output
--
-- @
-- ghci> test "code-examples/01-basics/hello-world.gloom"
-- === Phase 1: Static Analysis (Analysing) ===
-- ✅ Static analysis passed!
--
-- === Phase 2: Execution (Running) ===
-- Hello, World!
-- ✅ Program executed successfully!
-- @
test :: String -> IO ()
test filePath = do
  putStrLn $ "=== Phase 1: Static Analysis (Analysing) ==="
  tokens1 <- getTokens filePath
  let analysingState = freshState  -- Starts in Analysing phase
  result1 <- runParserT (program >> getState) analysingState filePath tokens1
  
  case result1 of
    Left err -> do
      putStrLn "\n❌ STATIC ANALYSIS FAILED:"
      print err
    Right finalState1 -> do
      putStrLn "✅ Static analysis passed!\n"
      
      putStrLn "=== Phase 2: Execution (Running) ==="
      tokens2 <- getTokens filePath
      -- Use the state from phase 1, but change flag to Running
      let runningState = finalState1 { rtFlag = Running }
      result2 <- runParserT program runningState filePath tokens2
      
      case result2 of
        Left err -> do
          putStrLn "\n❌ EXECUTION FAILED:"
          print err
        Right () -> do
          putStrLn "\n✅ Program executed successfully!"

-- | Test type definition parsing (structs and variants)
--
-- @
-- ghci> typε "test-struct.gloom"
-- @
typε :: String -> IO ()
typε filePath = do
  tokens <- getTokens filePath

  result <- runParserT Type.def freshState filePath tokens

  case result of
    Left err -> do
      print "FALHOU"
      print err
    Right _ -> do
      print "SUCESSO"
      print "Estado final:"

-- | Test expression parsing and evaluation
--
-- Useful for quick expression testing without a full program.
--
-- @
-- ghci> expr "2 + 3 * 4"
-- ghci> expr "[1, 2, 3, 4]"
-- @
expr :: String -> IO ()
expr input = do
  let tokens = alexScanTokens input
  result <- runParserT Parser.expr freshState "" tokens 

  case result of
    Left err -> do
      print "FALHOU"
      print err
    Right info -> do
      print $ fst info
      print . fmap tokenType . snd $ 
        info

-- | Test statement parsing
--
-- Parse and analyze multiple statements from a file.
--
-- @
-- ghci> stmts "test-statements.gloom"
-- @
stmts :: String -> IO ()
stmts filePath = do
  tokens <- getTokens filePath
  let parser = many Parser.stmt <* eof
  result <- runParserT parser freshState "" tokens 

  case result of
    Left err -> do
      print "FALHOU"
      print err
    Right info -> do
      -- print . fst . concat $ info
      print . fmap tokenType . concat $ info





