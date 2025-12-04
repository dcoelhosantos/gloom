




{-|
Module      : Parser.TopLevel
Description : Top-level declaration parser for Gloom programs
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module handles parsing of top-level declarations in Gloom programs:

* Function definitions (@fn@)
* Type definitions (@struct@, @variant@)
* Global variable declarations (@var@)
* Global constant declarations (@const@)

The 'program' parser is the entry point for parsing complete Gloom source files.
-}
module Parser.TopLevel
  ( -- * Program Parsing
    program

    -- * Top-level Declaration Parsing
  , topLevel
  ) where
-- global imports
import Control.Monad
import qualified Data.Set as Set
import Text.Parsec hiding (tokens, token)

-- local imports
import qualified Parser.Types as Type
import Parser.General
import Lexing.Lexer (Token(..), TokenType(..), prettyToken)
import Parser.Fun
import qualified Parser.Import as Import

-- | Parse a single top-level declaration
--
-- Disambiguates between different declaration types by looking at the first keyword.
-- This prevents trying multiple parsers and getting confusing error messages.
--
-- = Supported Top-Level Declarations
--
-- * @fn@ - Function definition ('pFun')
-- * @struct@ or @variant@ - Type definition ('Type.def')
-- * @var@ - Global variable ('gDeclVar')
-- * @const@ - Global constant ('gDeclConst')
--
-- == Example
--
-- @
-- fn main() {
--   print("Hello!");
-- }
--
-- struct Point { x: int, y: int }
--
-- const PI: float = 3.14;
-- var counter: int = 0;
-- @
topLevel :: Interpreter ()
topLevel = do
  firstToken <- lookAhead anyToken
  case tokenType firstToken of
    Import   -> void Import.parseImportStmt
    Function -> void pFun
    Struct   -> Type.def
    Var      -> gDeclVar
    Const    -> gDeclConst
    _ -> fail $ "Unexpected token at top level: " ++ prettyToken firstToken

-- | Parse a complete Gloom program
--
-- Entry point for parsing entire source files. 
-- 
-- Processing order:
-- 1. Parse all @import@ statements at the top
-- 2. Process imports (load files, detect cycles, merge scopes)
-- 3. Parse remaining top-level declarations
-- 4. Execute main function if present
--
-- This is the parser used by both 'Main.main' and 'Parser.File.test'.
program :: Interpreter ()
program = do
  -- Get current source file path from parser position
  pos <- getPosition
  let sourceFile = sourceName pos
  
  -- Parse and process imports
  -- This will recursively load all imported files and add their types/functions to scope
  void $ Import.processImports sourceFile Set.empty
  
  -- Parse remaining top-level declarations
  void $ many topLevel <* eof
  
