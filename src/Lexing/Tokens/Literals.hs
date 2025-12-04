{-|
Module      : Lexing.Tokens.Literals
Description : Literal value token parsers for Gloom
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module provides parsers for Gloom literal values.
Each parser extracts the literal value from a token and returns
it along with the token itself for error reporting.

= Supported Literals

* 'int' - Integer literals (@42@, @-17@, @0@)
* 'float' - Floating-point literals (@3.14@, @-0.5@, @2.0@)
* 'textLit' - String literals (@"hello"@, @"world"@)
* 'boolLit' - Boolean literals (@true@, @false@)
* 'nullLit' - Null reference (@null@)
* 'unitLit' - Unit/void literal (@()@)
* 'literal' - Combined parser returning ('Value', 'Typε') pair

== Examples

@
42        -- IntLit 42
3.14      -- FloatLit 3.14
"hello"   -- TextLit "hello"
true      -- BoolLit True
null      -- Null
()        -- HappyFace (Unit)
@
-}
module Lexing.Tokens.Literals
  ( -- * Basic Literals
    int
  , float
  , textLit
  , boolLit
  , nullLit
  , unitLit

    -- * Combined Literal Parser
  , literal
  ) where

-- global
import Text.Parsec hiding (token)
import qualified Data.Text as Text

-- local
import AST
import Lexing.Tokens.General (Parser, updatePos)
import Lexing.Lexer (Token(..), TokenType(..), prettyToken)

-- | Parse an integer literal token
--
-- Returns the integer value and the token.
-- Matches tokens like @42@, @-17@, @0@.
int :: Parser u (Integer, Token)
int = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token (IntLit x) _ _) = Just (x, token)
  get_token _ = Nothing

-- | Parse a floating-point literal token
--
-- Returns the float value and the token.
-- Matches tokens like @3.14@, @-0.5@, @2.0@.
float :: Parser u (Double, Token)
float = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token (FloatLit x) _ _) = Just (x, token)
  get_token _ = Nothing

-- | Parse a text (string) literal token
--
-- Returns the string value and the token.
-- Matches tokens like @"hello"@, @"world"@.
textLit :: Parser u (String, Token)
textLit = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token (TextLit s) _ _) = Just (s, token)
  get_token _ = Nothing

-- | Parse a boolean literal token
--
-- Returns the boolean value and the token.
-- Matches @true@ or @false@.
boolLit :: Parser u (Bool, Token)
boolLit = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token (BoolLit b) _ _) = Just (b, token)
  get_token _ = Nothing

-- | Parse a null literal token
--
-- Returns the token. Null represents an uninitialized or empty reference.
nullLit :: Parser u Token
nullLit = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token Null _ _) = Just token
  get_token _ = Nothing

-- | Parse a unit literal token (@\@@)
--
-- Returns the token. Unit represents "void" or "no value" in Gloom.
-- Used in functions that don't return anything, or as a placeholder value.
--
-- Examples:
-- @
-- var x: void = @;
-- return @;
-- @
unitLit :: Parser u Token
unitLit = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token UnitLit _ _) = Just token
  get_token _ = Nothing

-- | Parse any literal and return its runtime 'Value' and 'Typε'
--
-- This is a unified parser that handles all literal types and
-- automatically infers their types:
--
-- @
-- 42       → (VInt 42, TInt)
-- 3.14     → (VFloat 3.14, TFloat)
-- "hello"  → (VText "hello", TText)
-- true     → (VBool True, TBool)
-- null     → (VNull, TAny)
-- \@        → (VUnit, TUnit)
-- @
--
-- Note: @null@ has type 'TAny' since it can represent any reference type.
literal :: Parser u ((Value, Typε), Token)
literal = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token tokenType _ _) = 
    case tokenType of 
      BoolLit b    -> Just ((VBool b, TBool), token)
      IntLit n     -> Just ((VInt n, TInt), token)
      FloatLit x   -> Just ((VFloat x, TFloat), token)
      TextLit text -> Just ((VText $ Text.pack text, TText), token)
      Null         -> Just ((VNull, TRef TAny), token)  -- null is a reference to any type
      UnitLit      -> Just ((VUnit, TUnit), token) -- @ is the unit value
      _ -> Nothing