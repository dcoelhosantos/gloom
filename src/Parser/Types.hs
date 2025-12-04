{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Parser.Types
Description : Type definition parser for Gloom (struct and variant)
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module handles parsing of user-defined types in Gloom:

* Structs - Product types with named fields
* Variants - Sum types with named constructors (currently commented out)

Type definitions are added to the runtime state's type map during parsing.

== Example Struct

@
struct Point {
  x: int;
  y: int;
}
@

== Example Variant (future)

@
variant Option {
  Some(int);
  None;
}
@
-}
module Parser.Types
  ( -- * Type Definitions
    def
  , structDef
  ) where

-- global
--
import Control.Monad
import Text.Parsec hiding (tokens)

-- local
import Parser.General

import qualified Lexing.Tokens.Identifiers as Token
import qualified Lexing.Tokens.Symbols as Token
import qualified Lexing.Tokens.Keywords as Token
import RTState

-- | Parse a type definition (currently only struct)
--
-- Future: will support variants as well
def :: Interpreter ()
def = choice [ structDef ]

-- | Parse a struct definition
--
-- Structs are product types with named fields:
--
-- @
-- struct Point {
--   x: int;
--   y: int;
-- }
-- @
--
-- = Validation
--
-- * All field names must be unique
-- * Field types must be valid (either primitive or previously defined custom types)
--
-- The struct is added to the runtime state's type map after successful parsing.
structDef :: Interpreter ()
structDef = do 
  _ <- Token.struct
  -- Only check for fresh type name in Analysing phase
  -- In Running phase, types are already declared from Phase 1
  state <- getState
  (sIdent, _sIdent_T) <- if not (isRunning state)
    then freshTIdent =<< Token.typeId
    else Token.typeId

  -- TODO: remember why??
  _ <- Token.openBrace <* when (not $ isRunning state) (addStruct sIdent [])

  fields_ <- many $ do 
    (fieldIdent, fieldIdent_T) <- Token.varId
    colon_T <- Token.colon
    (field_type, field_type_T) <- typeOk =<< Token.typε
    semiColon_T <- Token.semiColon
    let tokens = [ fieldIdent_T 
                 , colon_T
                 ] ++ field_type_T
                 ++ [semiColon_T ]
           
    pure ((fieldIdent, field_type), tokens)

  let (fields, _fields_Ts) = handler fields_


  _ <- Token.closeBrace;

  when (not $ isRunning state) $ do
    unless (noReps $ fmap fst fields) . 
      fail $ "some fields are repeated in: " ++ show fields;
      -- TODO: improve this failMsg

    addStruct sIdent fields;

  -- 
  _ <- getState
  -- liftIO $ print rtTypes 
  pure ()

  where
    handler = mapSnd concat . unzip 
    mapSnd f (x, y) = (x, f y)
