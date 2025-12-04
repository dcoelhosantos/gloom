{-|
Module      : Lexing.Tokens.General
Description : General token parsing utilities for Gloom
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module provides the foundational token parsing infrastructure
used by all other token parsers. It defines the base 'Parser' type
and utility functions for working with the token stream.

= Core Types

* 'Parser' - The token parser monad transformer

= Utilities

* 'updatePos' - Update source position during parsing
* 'match' - Match a specific token type
* 'getNextToken' - Consume any token

These utilities are built on top of Parsec and work with the token
stream produced by "Lexing.Lexer".
-}
module Lexing.Tokens.General
  ( -- * Parser Type
    Parser

    -- * Token Matching Utilities
  , match
  , getNextToken
  , updatePos
  ) where

-- global
import Text.Parsec hiding (token, sourceName)
import Text.Parsec.Pos (newPos, sourceName)

-- local
import Lexing.Lexer (Token(..), TokenType(..), prettyToken)

-- | The base parser type for all token parsers
--
-- This is a Parsec parser that:
--
-- * Consumes a stream of 'Token's
-- * Carries user state of type @u@
-- * Runs in the IO monad
-- * Produces results of type @α@
--
-- All parsers in "Lexing.Tokens.*" modules use this type.
type Parser u α = ParsecT [Token] u IO α

-- | Update source position based on the current token
--
-- Extracts line and column information from the token to provide
-- accurate error locations in parse errors.
updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos pos token _ = 
    newPos (sourceName pos) (tokenLine token) (tokenCol token)

-- | Match a specific token type
--
-- This is the fundamental building block for all keyword and symbol parsers.
-- Returns the token if it matches the expected type, fails otherwise.
--
-- == Example
--
-- @
-- semicolon :: Parser u Token
-- semicolon = match SemiColon
-- @
match :: TokenType -> Parser u Token
match ttype = tokenPrim prettyToken updatePos get_token
  where
    get_token token@(Token tt _ _)
      | tt == ttype = Just token
      | otherwise   = Nothing

-- | Consume and return the next token, regardless of its type
--
-- Useful for lookahead or when you need to examine a token
-- without knowing its type in advance.
getNextToken :: Parser u Token
getNextToken = tokenPrim prettyToken updatePos Just
