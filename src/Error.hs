{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Error
Description : Error types and error handling for the Gloom compiler
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module defines the error hierarchy for Gloom compilation and runtime errors.
Errors are divided into two main categories:

* 'SyntaxError' - Parsing errors (invalid syntax, unexpected tokens)
* 'SemanticError' - Type checking and semantic analysis errors

The 'Error' type integrates with Megaparsec's error reporting system.
-}
module Error
  ( -- * Error Types
    Error(..)
  , SyntaxError(..)
  , SemanticError(..)

    -- * Message Type
  , Msg
  ) where

-- global 
import Data.Text
import Text.Megaparsec

-- | Error message text
type Msg = Text

-- | Syntax errors encountered during parsing
--
-- These errors occur when the source code doesn't match the expected grammar.
data SyntaxError
  = NotKeyword Msg  -- ^ Expected a keyword but got something else
  | A               -- ^ Placeholder error (legacy)
  deriving (Eq, Show, Ord)

-- | Semantic errors encountered during type checking and analysis
--
-- These errors occur when the code is syntactically valid but semantically incorrect.
--
-- = Type Errors
-- * 'TypeError' - Type mismatch or invalid type operation
--
-- = Declaration Errors
-- * 'NotDeclVar' - Variable used before declaration
-- * 'NotDeclFun' - Function called before declaration
-- * 'AlreadyDeclVar' - Variable declared twice in same scope
-- * 'AlreadyDeclFun' - Function declared twice
--
-- = Constructor Errors
-- * 'NotFoundConstructor' - Variant constructor doesn't exist
-- * 'ProblemWithArgs' - Function/constructor called with wrong number or types of arguments
data SemanticError
  = TypeError Msg            -- ^ Type mismatch or invalid operation
  | NotDeclVar Msg           -- ^ Undeclared variable
  | NotDeclFun Msg           -- ^ Undeclared function
  | AlreadyDeclVar Msg       -- ^ Variable redeclaration
  | AlreadyDeclFun Msg       -- ^ Function redeclaration
  | NotFoundConstructor Msg  -- ^ Unknown variant constructor
  | ProblemWithArgs Msg      -- ^ Argument count or type mismatch
  deriving (Eq, Show, Ord)
 
-- | Top-level error type
--
-- Combines all possible error types plus a catch-all 'Boom' for unexpected failures.
data Error
  = Syntax   SyntaxError    -- ^ Syntax/parsing error
  | Semantic SemanticError  -- ^ Semantic/type error
  | Boom                    -- ^ Unexpected catastrophic error
  deriving (Eq, Show, Ord)

-- | Megaparsec integration for custom error messages
instance ShowErrorComponent Error where
  showErrorComponent (Syntax  (NotKeyword txt)) = unpack txt ++ " is not a keyword"
  showErrorComponent _ = "* Custom error occurred during parsing"



