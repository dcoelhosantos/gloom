{-|
Module      : Lexing.Tokens.Identifiers
Description : Identifier and type token parsers for Gloom
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module provides parsers for identifiers and type annotations in Gloom.

= Identifier Parsers

* 'typeId' - Type names (capitalized): @Point@, @Option@
* 'varId' - Variable names (lowercase): @x@, @counter@

= Type Parsers

* 'typε' - Complete type parser (tries all type forms)
* 'atomicType' - Primitive and custom types: @int@, @bool@, @Point@
* 'arrayType' - Array types: @[int]@, @[[float]]@
* 'refType' - Reference types: @&int@, @&Point@

== Type Syntax Examples

@
int           -- Primitive integer
bool          -- Primitive boolean
Point         -- Custom type
[int]         -- Array of integers
[[float]]     -- 2D array of floats
&int          -- Reference to integer
&Point        -- Reference to custom type
[&int]        -- Array of integer references
@
-}
module Lexing.Tokens.Identifiers
  ( -- * Identifier Parsers
    typeId
  , varId

    -- * Type Parsers
  , typε
  , atomicType
  , arrayType
  , refType
  ) where 
--
-- global

import Text.Parsec hiding (token)

-- local
import Lexing.Tokens.General (Parser, updatePos)
import Lexing.Lexer (Token(..), TokenType(..), prettyToken)
import AST

import qualified Lexing.Tokens.Symbols as Sym

-- | Parse a type identifier (capitalized)
--
-- Type identifiers start with uppercase and refer to custom types:
--
-- @
-- Point      -- struct or variant name
-- Option     -- variant type
-- MyStruct   -- user-defined struct
-- @
typeId :: Parser u (Ident, Token)
typeId = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token (TypeId ident) _ _) = Just (ident, token)
  get_token _ = Nothing

-- | Parse a variable identifier (lowercase)
--
-- Variable identifiers start with lowercase and refer to variables,
-- functions, or struct fields:
--
-- @
-- x          -- variable name
-- counter    -- variable name
-- factorial  -- function name
-- @
varId :: Parser u (Ident, Token)
varId = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token (VarId ident) _ _) = Just (ident, token)
  get_token _ = Nothing

-- | Parse any type annotation
--
-- Tries to parse array types, reference types, or atomic types.
-- Returns the 'Typε' and the list of tokens consumed.
typε :: Parser u (Typε, [Token])
typε = choice 
  [ try arrayType 
  , try refType 
  , atomicType
  ]

-- | Parse an array type
--
-- Array types are written with brackets: @[element_type]@
--
-- @
-- [int]      -- TDynArray TInt
-- [[bool]]   -- TDynArray (TDynArray TBool)
-- [Point]    -- TDynArray (TCustom "Point")
-- @
arrayType :: Parser u (Typε, [Token])
arrayType = do
  startT      <- Sym.openBracket      
  (innerT, innerT_Ts) <- typε                 
  endT        <- Sym.closeBracket     
  pure (TDynArray innerT, startT : innerT_Ts ++ [endT])

-- | Parse a reference type
--
-- Reference types are written with @&@: @&type@
--
-- @
-- &int       -- TRef TInt
-- &Point     -- TRef (TCustom "Point")
-- &[int]     -- TRef (TDynArray TInt)
-- @
refType :: Parser u (Typε, [Token])
refType = do
  startT      <- Sym.ampersand       
  (innerT, innerT_Ts) <- typε                
  pure (TRef innerT, startT:innerT_Ts)

-- | Parse an atomic (non-composite) type
--
-- Atomic types are primitives or custom type names:
--
-- = Primitive Types
-- * @int@ - 'TInt'
-- * @float@ - 'TFloat'
-- * @text@ - 'TText'
-- * @bool@ - 'TBool'
-- * @void@ - 'TUnit'
--
-- = Custom Types
-- * @Point@, @Option@, etc. - 'TCustom' with the type name
atomicType :: Parser u (Typε, [Token])
atomicType = tokenPrim prettyToken updatePos get_token where
  get_token token@(Token tokenType _ _) = 
    case tokenType of
      TypeId ident -> Just (TCustom ident, [token])
      IntType   -> Just (TInt, [token])
      FloatType -> Just (TFloat, [token])
      TextType  -> Just (TText, [token])
      BoolType  -> Just (TBool, [token])
      VoidType  -> Just (TUnit, [token])
      _  -> Nothing
