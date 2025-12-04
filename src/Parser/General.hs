{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | General parsing utilities and interpreter infrastructure.
--
-- This module provides:
--
-- * Core types: 'Interpreter' monad, 'ValueInfo' tuples
-- * Variable management: 'addVar', 'searchVarInfo', 'updateVarAt'
-- * Type validation: 'typeOk', 'unifyParams', 'searchCustomType'
-- * Scope control: 'enterBlock', 'quitBlock', 'enterScope'
-- * State manipulation: 'turnOn', 'turnOff', 'freshTIdent'
-- * Helper utilities: 'sepBy\'', 'noReps', 'defaultValue'
--
-- Most parser phases use 'Interpreter' to access 'RTState' during parsing.

module Parser.General
  ( -- * Type Aliases
    Interpreter
  , ValueInfo
    
    -- * Parsing Utilities
  , sepBy'
  , noReps
    
    -- * Variable Management
  , addVar
  , addGlobalVar
  , updateVarAt
  , updateGlobalVar
  , assertVarFreshness
  , currentPrefix
    
    -- * Variable Lookup
  , searchVarInfo
  , searchVarInfoWithToken
  , searchVarInfoForAssignment
  , searchVarInfoAt
  , searchVarValue
  , searchVarValueWithToken
  , searchVarValueAt
    
    -- * Function Management
  , addFun
  , addFunWithToken
  , searchFun
    
    -- * Type System
  , addStruct
  , searchCustomType
  , searchStruct
  , typeOk
  , freshTIdent
    
    -- * Value Utilities
  , defaultValue
  , placeholderValue
    
    -- * Scope Control
  , enterBlock
  , enterScope
  , quitBlock
  , stackUp
    
    -- * State Control
  , turnOn
  , turnOff
    
    -- * Type Checking
  , unifyParams
  , fopTypeMismatch
  ) where


-- global
import Control.Monad
import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Trans  -- Unused
import Data.Maybe
import qualified Data.Map as Map
-- import qualified Data.Char as Char  -- Unused
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.List as List
import qualified Data.List as String
import qualified Data.Vector as Vector
-- import qualified Data.List.NonEmpty as NonEmpty


import Lexing.Lexer
import AST
import qualified Error.Formatter as ErrFmt

import RTState
import Text.Parsec hiding (token, tokens, label, count)
-- import Control.Monad.IO.Class

import Control.Lens hiding ((<|))

--local 

import Lexing.Tokens.General

-- | A runtime value paired with its type (used during evaluation).
type ValueInfo = (Value, Typε)

-- instance Show ValueInfo where
    -- show (val, ty) = show val ++ " :: " ++ show ty

-- | The parser monad with access to runtime state.
--
-- All semantic analysis (type checking, variable lookup, scope management)
-- uses this monad to read/modify 'RTState'.
type Interpreter α = ParsecT [Token] RTState IO α

-- | Like 'sepBy', but also captures token sequences.
--
-- Returns both parsed values and all tokens consumed (including separators).
--
-- @
-- sepBy' parser Token.comma
-- @
sepBy' :: Parser u (α, [Token]) -> Parser u Token -> Parser u ([α], [Token])
parser `sepBy'` token = do {
  (val, tokensP) <- parser;
  outs <- many $ do {
    tokenT <- token;
    (val', tokensP') <- parser;
    pure (val', tokenT:tokensP');
  };

  let  -- TODO: check if the order is correct!!!
    handler [] = ([], [])
    handler ((val', tokensP'):outs') = (val':vals', tokensP'++tokens')
      where (vals', tokens') = handler outs'
    (vals, tokens) = handler outs
  in
    pure (val:vals, tokensP++tokens)
} <|> pure ([], [])

-- | Verifies that a variable name is not already declared in the current scope.
--
-- Throws a parser error if the identifier exists in:
--
-- * Global scope (if at top level)
-- * Current local scope (if inside a function)
--
-- This enforces Gloom's variable shadowing rules.
assertVarFreshness :: Ident -> Interpreter ()
assertVarFreshness vIdent = do
  RTState {..} <- getState
  case rtStack of
    [] -> do 
      when (vIdent `Map.member` rtGlobals) $
        fail $ "global_var already declared: " ++ vIdent

    (Scope _ current_scope, _):_ -> do
      when (vIdent `Map.member` current_scope) $
        fail $ "var already declared: " ++ vIdent

-- | Gets the current scope's prefix (or Nothing if at global scope).
--
-- The prefix is used to disambiguate variables with the same name
-- across different scopes/blocks.
currentPrefix :: Interpreter (Maybe Prefix)
currentPrefix = do
  RTState {..} <- getState
  case rtStack of
    [] -> pure Nothing
    (Scope current_prefix _, _):_ ->
      pure $ Just current_prefix


-- | Updates a variable's value at a specific prefix (scope).
--
-- Also marks the variable as initialized.
--
-- @
-- updateVarAt "foo!1" "x" (VInt 42)
-- @
updateVarAt :: Prefix -> Ident -> Value -> Interpreter ()
updateVarAt prefix ident value = do
  VarInfo _mValue _vType is_const _wasInit <- searchVarInfoAt prefix ident
  
  -- NOTE: Type checking is done at parse time by type inference system
  -- The value passed here has already been validated by expr/assignment parsers
  -- Future improvement: Add runtime type assertion for safety
  -- TODO: Add explicit type check: when (typeOf value /= vType) $ fail ...
  
  when is_const . fail $
    "error: can't modify the const "
    ++ ident

  -- Update value AND mark as initialized
  let aux var_info = var_info { varValue = Just value, wasInitialized = True }

  case prefix of 
    -1 ->  updateState $
      globals %~
      Map.adjust aux ident 
      
    _   -> 
      let update (Scope current_prefix table, has_link) = 
           (,has_link) . Scope current_prefix $

           if current_prefix == prefix
           then Map.adjust aux ident table
           else table in
      updateState $
        stack %~
        fmap update

-- | Adds a variable to the current scope (or globals if no scope exists).
--
-- This is the primary function for variable declaration in Gloom. It handles:
--
-- * Variable name uniqueness checking
-- * Automatic default initialization for structs, arrays, and references
-- * Initialization tracking (for use-before-init errors)
-- * Assignment to appropriate scope (local stack or global table)
-- * Unique prefix generation for variable tracking
--
-- ==== __Parameters__
--
-- [@vIdent@] Variable name (must be unique in current scope)
-- [@typε@] Declared type of the variable
-- [@mValue@] Optional initial value (Nothing for declarations without @= value@)
-- [@const_flag@] Whether variable is immutable (@const@)
--
-- ==== __Return Value__
--
-- Returns the variable's unique 'Prefix', which identifies the variable across scopes.
-- This prefix is used to track the variable through assignments, reads, and scope exits.
--
-- ==== __Examples__
--
-- @
-- -- Simple integer variable
-- addVar "count" TInt Nothing False
-- -- Creates: int count;  (uninitialized)
--
-- -- Initialized constant
-- addVar "PI" TFloat (Just (VFloat 3.14159)) True
-- -- Creates: const float PI = 3.14159;
--
-- -- Struct with auto-initialization
-- addVar "person" (TCustom "Person") Nothing False
-- -- Creates: Person person = Person { name = "", age = 0 };
--
-- -- Array with auto-initialization
-- addVar "numbers" (TDynArray TInt) Nothing False
-- -- Creates: int[] numbers = [0];  (single-element default array)
--
-- -- Reference (nullable)
-- addVar "ptr" (TRef TInt) Nothing False
-- -- Creates: &int ptr = null;
-- @
--
-- ==== __Automatic Initialization__
--
-- Some types receive default values automatically even without an explicit initializer:
--
-- * __Structs__ ('TCustom'): All fields set to their type's default value
-- * __Arrays__ ('TDynArray'): Single-element array containing default value
-- * __References__ ('TRef'): Initialized to @null@
-- * __Primitives__: No auto-initialization (use-before-init error if accessed)
--
-- ==== __Initialization Tracking__
--
-- The 'wasInitialized' flag in 'VarInfo' is set based on:
--
-- 1. Explicit value provided (@mValue@ is 'Just')
-- 2. Auto-generated default (struct/array/reference)
--
-- This flag is checked during the 'Analysing' phase to catch use-before-init errors.
--
-- ==== __Error Cases__
--
-- Fails with error message if:
--
-- * Variable name already exists in current scope (detected by 'assertVarFreshness')
-- * Struct type name not found (invalid 'TCustom' name)
--
-- ==== __Side Effects__
--
-- Modifies the interpreter state by:
--
-- * Adding variable to current scope's symbol table (or 'rtGlobals' if no scope)
-- * Incrementing 'freshPrefix' counter for unique variable IDs
-- * Potentially looking up struct definition in 'rtTypes' for default initialization
--
-- ==== __Global vs Local__
--
-- * If scope stack is empty → variable added to 'rtGlobals' (prefix = -1)
-- * If scope stack exists → variable added to topmost scope's symbol table
--
-- See @code-examples/01-basics/const-vars.gloom@ and @globals.gloom@ for usage examples.
--
-- @since 0.1.0
addVar :: Ident -> Typε -> Maybe Value -> Bool -> Interpreter Prefix
addVar vIdent typε mValue const_flag = do
  assertVarFreshness vIdent
  
  -- Create default values for structs and arrays  
  value_to_put <- case mValue of
    Just v -> pure $ Just v
    Nothing -> case typε of
      TCustom sName -> do
        -- Create struct with all fields initialized with defaults
        typeDef <- searchStruct sName
        case typeDef of
          DStruct _ fieldsMap -> do
            -- Initialize each field with its default
            fieldsWithDefaults <- forM (Map.toList fieldsMap) $ \(fieldName, fieldType) -> do
              defVal <- defaultValue fieldType
              pure (fieldName, (Just defVal, fieldType))
            pure $ Just $ VStruct sName (Map.fromList fieldsWithDefaults)
      TDynArray elemType -> do
        -- Arrays default to single-element array
        defVal <- defaultValue elemType
        pure $ Just $ VDynArray (Vector.singleton defVal)
      TRef _ -> do
        -- References default to null
        pure $ Just VNull
      _ -> pure Nothing

  -- Variable is initialized if:
  -- 1. It has a value explicitly provided (mValue is Just), OR
  -- 2. We auto-generated a default for struct/array (value_to_put is Just)
  let was_initialized = isJust value_to_put

  RTState {..} <- getState
  case rtStack of
    [] -> do 
      updateState $ 
        globals %~ 
        Map.insert vIdent 
        (VarInfo value_to_put typε const_flag was_initialized) 
      pure (-1)
    (Scope label current_scope, has_link) : other_scopes -> do
      let updated_scope =
           current_scope &
           Map.insert vIdent
           (VarInfo value_to_put typε const_flag was_initialized)

      updateState $
        stack .~
        (Scope label updated_scope, has_link) 
          : other_scopes
      pure label

-- | Adds a global variable to 'rtGlobals' (bypasses local scope stack).
--
-- Unlike 'addVar', this always writes to the global scope, even when
-- called from inside a function.
--
-- In the 'Analysing' phase, fails if the variable already exists.
-- In the 'Running' phase, allows re-declaration (for two-pass execution).
--
-- @
-- addGlobalVar "VERSION" TInt (Just (VInt 1)) True
-- @
addGlobalVar :: Ident -> Typε -> Maybe Value -> Bool -> Interpreter ()
addGlobalVar vIdent typε mValue const_flag = do
  RTState {..} <- getState
  
  -- Check if already exists in globals - allow re-declaration in Running phase
  when (vIdent `Map.member` rtGlobals && rtFlag == Analysing) $
    fail $ "global var already declared: " ++ vIdent
  
  -- Only add if not already declared (allows re-parsing in Running phase)
  unless (vIdent `Map.member` rtGlobals) $ do
    -- Create default values for structs and arrays
    value_to_put <- case mValue of
      Just v -> pure $ Just v
      Nothing -> case typε of
        TCustom sName -> do
          typeDef <- searchStruct sName
          case typeDef of
            DStruct _ fieldsMap -> do
              fieldsWithDefaults <- forM (Map.toList fieldsMap) $ \(fieldName, fieldType) -> do
                defVal <- defaultValue fieldType
                pure (fieldName, (Just defVal, fieldType))
              pure $ Just $ VStruct sName (Map.fromList fieldsWithDefaults)
        TDynArray elemType -> do
          -- Arrays default to single-element array
          defVal <- defaultValue elemType
          pure $ Just $ VDynArray (Vector.singleton defVal)
        TRef _ -> do
          -- References default to null
          pure $ Just VNull
        _ -> pure Nothing
    
    -- Global is initialized if we have a value (explicit or auto-generated)
    let was_initialized = isJust value_to_put
    
    updateState $ 
      globals %~ 
      Map.insert vIdent (VarInfo value_to_put typε const_flag was_initialized)

-- | Updates a global variable's value (used during 'Running' phase initialization).
--
-- Fails if:
--
-- * The variable doesn't exist
-- * The variable is marked as @const@
--
-- @
-- updateGlobalVar "counter" (VInt 0)
-- @
updateGlobalVar :: Ident -> Value -> Interpreter ()
updateGlobalVar vIdent value = do
  RTState {..} <- getState
  case Map.lookup vIdent rtGlobals of
    Nothing -> fail $ "Global variable not found: " ++ vIdent
    Just (VarInfo _ typε constFlag _wasInit) -> do
      when constFlag $ fail $ "Cannot modify const global: " ++ vIdent
      -- Mark as initialized when value is assigned
      updateState (globals %~ Map.insert vIdent (VarInfo (Just value) typε constFlag True))

-- | Registers a function definition (without a body).
--
-- In the 'Analysing' phase, fails if the function name is already declared.
-- In the 'Running' phase, silently skips re-declaration (for two-pass execution).
--
-- @
-- addFun "factorial" (TInt, [(TInt, "n")], Nothing)
-- @
addFun :: Ident -> FunAss -> Interpreter ()
addFun fun_ident fun_ass@(_, _params, Nothing) = do
  RTState {..} <- getState
  -- Allow re-declaration in Running phase (skip if already exists)
  if fun_ident `Map.member` rtFuns then
    if rtFlag == Running then
      pure ()  -- Skip re-declaration in Running phase
    else
      fail "* Function already declared" -- Error only in Analysing phase
  else 
    updateState $ 
      funs %~
      Map.insert fun_ident fun_ass

addFun fun_ident fun_ass@(typε, params, Just _) = do
  RTState {..} <- getState
  -- Allow re-declaration in Running phase (skip if already has body)
  case fun_ident `Map.lookup` rtFuns of 
    Just (fun_type, fun_params, Nothing) 
      | fun_type   /= typε   -> undefined
      | fun_params /= params -> undefined
      | otherwise -> updateState $ funs %~ Map.insert fun_ident fun_ass
    Just (_, _, Just _) -> 
      if rtFlag == Running then
        pure ()  -- Skip re-declaration in Running phase
      else
        fail "* Function already declared with a body" -- Error only in Analysing
    Nothing ->
      updateState $ 
        funs %~
        Map.insert fun_ident fun_ass

-- | Like 'addFun' but with professional error message when function is already declared.
addFunWithToken :: Ident -> Token -> FunAss -> Interpreter ()
addFunWithToken fun_ident token fun_ass@(_, _params, Nothing) = do
  RTState {..} <- getState
  if fun_ident `Map.member` rtFuns then
    if rtFlag == Running then
      pure ()
    else do
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0428]: function is already defined" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " fn " ++ fun_ident ++ "(...) { ... }\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "   " ++ String.replicate (length fun_ident) '^' ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "function '" ++ fun_ident ++ "' is already declared\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "remove duplicate function definition\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "or rename one of the functions\n"
      
      fail "* Function already declared"
  else 
    updateState $ funs %~ Map.insert fun_ident fun_ass

addFunWithToken fun_ident token fun_ass@(typε, params, Just _) = do
  RTState {..} <- getState
  case fun_ident `Map.lookup` rtFuns of 
    Just (fun_type, fun_params, Nothing) 
      | fun_type   /= typε   -> undefined
      | fun_params /= params -> undefined
      | otherwise -> updateState $ funs %~ Map.insert fun_ident fun_ass
    Just (_, _, Just _) -> 
      if rtFlag == Running then
        pure ()
      else do
        let line = tokenLine token
            col = tokenCol token
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0428]: function is already defined" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " fn " ++ fun_ident ++ "(...) { ... }\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "   " ++ String.replicate (length fun_ident) '^' ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "function '" ++ fun_ident ++ "' already has a body\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "remove duplicate function definition\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "or rename one of the functions\n"
        
        fail "* Function already declared with a body"
    Nothing ->
      updateState $ funs %~ Map.insert fun_ident fun_ass

-- | Registers a struct type definition.
--
-- @
-- addStruct "Person" [("name", TText), ("age", TInt)]
-- @
addStruct :: Ident -> [(Ident, Typε)] -> Interpreter ()
addStruct ident fields = updateState $
  types %~ Map.insert ident (DStruct ident $ Map.fromList fields)


-- | Validates that a type exists (recursively checks nested types).
--
-- Fails if any custom type ('TCustom') is not defined in 'rtTypes'.
--
-- Recursively validates:
--
-- * Array element types
-- * Reference target types
--
-- @
-- typeOk (TDynArray (TCustom "Person"), tokens)
-- @
typeOk :: (Typε, [Token]) -> Interpreter (Typε, [Token])
typeOk (typ, tokens) = do
  validateRecursively typ
  pure (typ, tokens)
  where
    validateRecursively :: Typε -> Interpreter ()
    validateRecursively t = case t of
      TCustom tIdent -> do
        state <- getState
        unless (Map.member tIdent (state^.types)) $
           fail $ "Unknown type: '" ++ tIdent ++ "'"

      TDynArray inner -> validateRecursively inner

      TRef inner -> validateRecursively inner

      _ -> pure ()

-- | Ensures a type identifier is not already declared.
--
-- Used when defining new structs to prevent name conflicts.
--
-- @
-- freshTIdent ("Person", token) -- OK if Person not defined
-- freshTIdent ("Int", token)    -- FAIL (Int is built-in)
-- @
freshTIdent :: (Ident, Token) -> Interpreter (Ident, Token)
freshTIdent (tIdent, token) = do
  state <- getState
  when (tIdent `Map.member` (state^.types))
    (fail $ "types already declared: " ++ tIdent)

  pure (tIdent, token)


-- | Checks if a list has no duplicate elements.
--
-- Used to validate struct fields and function parameters.
--
-- @
-- noReps ["a", "b", "c"] = True
-- noReps ["a", "b", "a"] = False
-- @
noReps :: Eq α => [α] -> Bool
noReps a = length a == length (List.nub a)

-- | Creates a default/zero value for a given type.
--
-- Recursively generates default values for:
--
-- * Primitives: @False@, @0@, @""@
-- * Structs: recursively defaults all fields
--
-- @
-- defaultValue TInt = VInt 0
-- defaultValue (TCustom "Person") = VStruct "Person" {...}
-- @
-- | Creates default/zero value for a given type.
--
-- Recursively generates default values for:
--
-- * Primitives: @False@, @0@, @0.0@, @""@
-- * Structs: recursively defaults all fields
-- * Arrays: single-element array with default value
--
-- @
-- defaultValue TInt = VInt 0
-- defaultValue (TCustom "Person") = VStruct "Person" {...}
-- @
defaultValue :: Typε -> Interpreter Value
defaultValue typε = do
  case typε of
    TBool -> pure $ VBool False

    TInt -> pure $ VInt 0
    TFloat -> pure $ VFloat 0.0
    TText -> pure $ VText Text.empty
    TUnit -> pure VUnit
    TCustom sIdent -> do
      -- Look up struct definition and create with default values for all fields
      typeDef <- searchStruct sIdent
      case typeDef of
        DStruct _ fieldsMap -> do
          -- Recursively create default values for each field
          defaultFields <- traverse defaultValue fieldsMap
          pure $ VStruct sIdent (Map.mapWithKey (\k v -> (Just v, fieldsMap Map.! k)) defaultFields)

    TDynArray innerType -> do
      -- Array default is single-element array (not empty) to avoid index errors
      innerDefault <- defaultValue innerType
      pure $ VDynArray (Vector.singleton innerDefault)
      
    TRef _innerType -> pure VNull  -- References default to null
    -- sei la
    TAny   -> pure $ VBool False -- fail "error: no default instance for TAny"

-- | Creates placeholder values for type checking in the 'Analysing' phase.
--
-- Unlike 'defaultValue', this never fails (returns empty structs for custom types).
--
-- Used to avoid recursive struct resolution during static analysis.
placeholderValue :: Typε -> Value
placeholderValue TInt = VInt 0
placeholderValue TFloat = VFloat 0.0
placeholderValue TBool = VBool False
placeholderValue TText = VText Text.empty
placeholderValue TUnit = VUnit
placeholderValue (TRef _) = VNull  -- Pointers default to null
placeholderValue (TDynArray _) = VDynArray Vector.empty
placeholderValue (TCustom name) = VStruct name Map.empty  -- Empty struct for analysis
placeholderValue TAny = VInt 0  -- Placeholder for unknown types


-- | Searches for a variable in the scope stack, returning its prefix and info.
--
-- This is the primary function for variable lookup during parsing and execution.
-- It implements Gloom's lexical scoping rules by searching through nested scopes
-- with proper shadowing behavior.
--
-- ==== __Lookup Order__
--
-- 1. __Current local scope__ (topmost stack frame)
-- 2. __Parent scopes__ (only if current scope has @has_link = True@)
-- 3. __Global scope__ (@rtGlobals@ with prefix = -1)
--
-- The @has_link@ flag controls whether a scope can see outer scopes. For example:
--
-- * Function bodies: @has_link = False@ (can only see parameters and globals)
-- * Block statements: @has_link = True@ (can see enclosing function's variables)
--
-- ==== __Parameters__
--
-- [@vIdent@] Variable name to search for
--
-- ==== __Return Value__
--
-- Returns a tuple @(prefix, varInfo)@ where:
--
-- * @prefix@ - The unique scope ID where variable was found (-1 for globals)
-- * @varInfo@ - Complete 'VarInfo' record (value, type, const flag, init status)
--
-- ==== __Examples__
--
-- @
-- -- Looking up a local variable:
-- (prefix, VarInfo (Just (VInt 42)) TInt False True) <- searchVarInfo "count"
--
-- -- Looking up a global:
-- (-1, VarInfo (Just (VText "Gloom")) TText True True) <- searchVarInfo "LANG_NAME"
--
-- -- Variable shadowing (finds innermost):
-- do
--   addVar "x" TInt (Just (VInt 10)) False  -- outer scope
--   enterBlock
--   addVar "x" TInt (Just (VInt 20)) False  -- inner scope (shadows outer)
--   (_, VarInfo (Just (VInt 20)) _ _ _) <- searchVarInfo "x"  -- finds inner
-- @
--
-- ==== __Scope Linking Example__
--
-- @
-- fn outer() {
--   var x = 10;
--   {  -- Block with has_link = True
--     print(x);  -- OK! Can see outer's x
--   }
-- }
--
-- fn inner() {
--   -- Function scope has has_link = False
--   print(x);  -- ERROR! Cannot see outer's x (unless it's global)
-- }
-- @
--
-- ==== __Error Cases__
--
-- Fails with error message if:
--
-- * Variable is not found in any accessible scope (local, parent, or global)
-- * Error message: @"* Variable not found in current scope or global scope"@
--
-- ==== __Side Effects__
--
-- * Read-only operation (does not modify interpreter state)
-- * Reads from 'rtStack' and 'rtGlobals'
--
-- ==== __See Also__
--
-- * 'searchVarInfoForAssignment' - Same lookup but for LHS of assignments
-- * 'searchVarInfoAt' - Lookup in specific scope by prefix
-- * 'addVar' - Add new variable to scope
--
-- @since 0.1.0
searchVarInfo :: Ident -> Interpreter (Integer, VarInfo)
searchVarInfo vIdent = do
  RTState { .. } <- getState
  findVar rtStack

  where
    findVar :: [(Scope, Bool)] -> Interpreter (Integer, VarInfo)
    findVar [] = do
      RTState { .. } <- getState
      maybe (fail "* Variable not found in current scope or global scope") 
        (pure . (-1,)) 
        (Map.lookup vIdent rtGlobals)
    findVar ((Scope label table, has_link):scopes) =
      case Map.lookup vIdent table of
        Just varInfo -> pure (label, varInfo)
        Nothing  
          | has_link  -> findVar scopes
          | otherwise -> findVar []

-- | Like 'searchVarInfo' but with professional error message when variable is not found.
-- Takes a Token to show precise error location.
searchVarInfoWithToken :: Ident -> Token -> Interpreter (Integer, VarInfo)
searchVarInfoWithToken vIdent token = do
  RTState { .. } <- getState
  result <- findVarMaybe rtStack
  case result of
    Just info -> pure info
    Nothing -> do
      -- Show professional error message
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0425]: cannot find variable" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " " ++ vIdent ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ String.replicate (length vIdent) '^' ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "cannot find variable '" ++ vIdent ++ "' in this scope\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "declare the variable before using it: " ++ 
        c ++ "var " ++ vIdent ++ ": type = value;" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "or check if the variable name is spelled correctly\n"
      
      fail "* Variable not found in current scope or global scope"

  where
    findVarMaybe :: [(Scope, Bool)] -> Interpreter (Maybe (Integer, VarInfo))
    findVarMaybe [] = do
      RTState { .. } <- getState
      pure $ fmap (-1,) (Map.lookup vIdent rtGlobals)
    findVarMaybe ((Scope label table, has_link):scopes) =
      case Map.lookup vIdent table of
        Just varInfo -> pure $ Just (label, varInfo)
        Nothing  
          | has_link  -> findVarMaybe scopes
          | otherwise -> findVarMaybe []

-- | Like 'searchVarInfo', but for use in assignment LHS.
--
-- Does NOT check initialization status, because we're about to initialize it!
-- This prevents the error "variable used before initialization" when doing:
--
-- @
-- var x: int;
-- x = 42;  -- This is OK! We're initializing it now
-- @
searchVarInfoForAssignment :: Ident -> Interpreter (Integer, VarInfo)
searchVarInfoForAssignment = searchVarInfo  -- Same implementation, just semantic distinction


-- | Searches for a variable at a specific prefix (scope).
--
-- Only looks in the exact scope identified by the prefix (no fallback).
--
-- @
-- searchVarInfoAt 42 "x" -- looks only in scope with prefix 42
-- @
searchVarInfoAt :: Prefix -> Ident -> Interpreter VarInfo
searchVarInfoAt p vIdent = do
  RTState { .. } <- getState
  maybe (fail "") pure (findVarAt $ fmap fst rtStack) 
  where
    findVarAt :: [Scope] -> Maybe VarInfo
    findVarAt [] = Nothing
    findVarAt (Scope label table:scopes) 
      | p == label = Map.lookup vIdent table
      | otherwise = findVarAt scopes

-- | Gets a variable's value and type at a specific prefix.
--
-- If the variable has no value, generates a default using 'defaultValue'.
--
-- @
-- searchVarValueAt 42 "x" -- returns (VInt 10, TInt)
-- @
searchVarValueAt :: Prefix -> Ident -> Interpreter ValueInfo
searchVarValueAt p vIdent = do
  VarInfo mValue typε _const_flag _wasInit <- searchVarInfoAt p vIdent
  case mValue of 
    Nothing -> do
      val <- defaultValue typε
      pure (val, typε)
    Just value -> 
      pure (value, typε)
  
-- | Gets a variable's value and type (searches from current scope upwards).
--
-- Behavior differs by phase:
--
-- * 'Analysing': **ERRORS** if variable is uninitialized (strict initialization check)
-- * 'Running': uses 'defaultValue' (fully initializes structs for backward compatibility)
--
-- @
-- searchVarValue "x" -- ERROR if x is uninitialized in Analysing phase
-- @
searchVarValue :: Ident -> Interpreter ValueInfo
searchVarValue vIdent = do
  (_, VarInfo mValue typε _const_flag _wasInit) <- searchVarInfo vIdent
  state <- getState
  
  -- NOTE: We don't check wasInit during either phase because:
  -- - Analysing: Sequential initialization analysis is complex due to interleaved parsing
  -- - Running: Variables are re-parsed, so wasInit flags reset but values persist
  -- Future: Implement proper data-flow analysis for compile-time init checking
  
  case mValue of
    Just value -> pure (value, typε)
    Nothing  -> do
      -- Use placeholder/default value for type checking
      -- The wasInit check above will catch uninitialized usage if needed
      value <- if isRunning state
               then defaultValue typε  -- Runtime: generate defaults
               else pure (placeholderValue typε)  -- Analysing: use placeholders
      pure (value, typε)

-- | Like 'searchVarValue' but with professional error message.
searchVarValueWithToken :: Ident -> Token -> Interpreter ValueInfo
searchVarValueWithToken vIdent token = do
  (_, VarInfo mValue typε _const_flag _wasInit) <- searchVarInfoWithToken vIdent token
  state <- getState
  
  case mValue of
    Just value -> pure (value, typε)
    Nothing  -> do
      value <- if isRunning state
               then defaultValue typε
               else pure (placeholderValue typε)
      pure (value, typε)


-- | Looks up a custom type (struct) by name.
--
-- @
-- searchCustomType "Person" -- returns DStruct "Person" {...}
-- @
searchCustomType :: Ident -> Interpreter TypeDef
searchCustomType tIdent = do
  state <- getState
  maybe (fail $ "custom type not found: " ++ show tIdent) pure $
    Map.lookup tIdent (state^.types)

-- | Looks up a struct definition (fails if the type is not a struct).
--
-- @
-- searchStruct "Person" -- OK if Person is a struct
-- searchStruct "Option" -- FAIL if Option does not exist
-- @
searchStruct :: Ident -> Interpreter TypeDef
searchStruct sIdent = do
  tDef <- searchCustomType sIdent
  case tDef of
    DStruct _ _ -> pure tDef

-- | Looks up a function definition by name.
--
-- Returns 'Nothing' if the function doesn't exist.
--
-- @
-- searchFun "factorial" -- returns Just (TInt, [(TInt, "n")], Nothing)
-- @
searchFun :: Ident -> Interpreter (Maybe FunAss)
searchFun fIdent = do
  state <- getState
  pure $ Map.lookup fIdent (state^.funs)


-- | Formats a type mismatch error message for binary operations.
--
-- @
-- fopTypeMismatch TInt TText "+" 
-- -- "TypeError: '+' not supported between types TInt and TText"
-- @
fopTypeMismatch :: Typε -> Typε -> String -> String
fopTypeMismatch tL tR operation = betterBinaryOpError tL tR operation
  where
    -- Helper functions
    isRefType (TRef _) = True
    isRefType _ = False
    
    betterBinaryOpError :: Typε -> Typε -> String -> String
    betterBinaryOpError tL' tR' opName
      | isRefType tL' && not (isRefType tR') = 
          "Type Error: Cannot use '" ++ opName ++ "' between reference type " 
          ++ show tL' ++ " and primitive type " ++ show tR'
      | not (isRefType tL') && isRefType tR' = 
          "Type Error: Cannot use '" ++ opName ++ "' between primitive type " 
          ++ show tL' ++ " and reference type " ++ show tR'
      | isRefType tL' && isRefType tR' =
          "Type Error: Cannot use '" ++ opName ++ "' between reference types " 
          ++ show tL' ++ " and " ++ show tR'
      | tL' == tR' =
          "Type Error: Operation '" ++ opName ++ "' not supported for type " ++ show tL'
      | otherwise =
          "Type Error: Operator '" ++ opName ++ "' not supported between types " 
          ++ show tL' ++ " and " ++ show tR'

-- | Internal helper: pushes a new scope onto the stack.
--
-- The @has_link@ parameter controls whether the new scope can see parent scopes.
stackUp :: Bool -> Interpreter ()
stackUp has_link = do 

  RTState {..} <- getState

  let new_scope = Scope freshPrefix Map.empty
  updateState refreshPrefix
  updateState $ stack %~ ((new_scope, has_link) :)

-- | Enters a new lexical scope with parent scope visibility.
--
-- This function creates a new scope frame on the stack with @has_link = True@,
-- meaning variables from parent scopes remain accessible (lexical scoping).
--
-- ==== __Use Cases__
--
-- * Control flow blocks: @if@, @while@, @for@ bodies
-- * Nested blocks within functions
-- * Any scope that should see enclosing variables
--
-- ==== __Scope Behavior__
--
-- With @has_link = True@:
--
-- @
-- var x = 10;
-- {
--   enterBlock  -- New scope sees x from parent
--   var y = 20;
--   print(x + y);  -- OK! x is visible (30)
-- }
-- @
--
-- ==== __Contrast with Function Scopes__
--
-- Function bodies use @stackUp False@ instead (no link to parent):
--
-- @
-- var x = 10;
-- fn test() {
--   -- Function scope has has_link = False
--   print(x);  -- ERROR! x not visible (unless global)
-- }
-- @
--
-- ==== __Side Effects__
--
-- Modifies interpreter state by:
--
-- * Creating new 'Scope' with fresh unique 'Prefix'
-- * Pushing scope onto 'rtStack' with @has_link = True@
-- * Incrementing 'freshPrefix' counter
--
-- ==== __Pairing__
--
-- Must be paired with 'exitBlock' (or 'quitBlock') to prevent scope leaks:
--
-- @
-- enterBlock
-- addVar "temp" TInt Nothing False
-- -- ... use temp ...
-- exitBlock  -- temp no longer accessible
-- @
--
-- ==== __See Also__
--
-- * 'exitBlock' - Removes current scope from stack
-- * 'quitBlock' - Alternative name (same behavior)
-- * 'stackUp' - Low-level function for custom @has_link@ values
-- * 'addVar' - Add variables to the new scope
--
-- @since 0.1.0
enterBlock :: Interpreter ()
enterBlock = stackUp True

-- | Enters a new isolated scope (no link to parent scope).
--
-- Variables from parent scopes are NOT visible (except globals).
--
-- Used for: pattern match branches (to isolate pattern variables).
enterScope :: Interpreter ()
enterScope = stackUp False

-- | Exits the current block/scope.
--
-- Pops the top frame from 'rtStack'.
--
-- Fails if the stack is already empty.
quitBlock :: Interpreter ()
quitBlock = do
  RTState {..} <- getState

  case rtStack of 
    [] -> do
      let y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E9999]: internal compiler error" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "attempted to quit a block, but the block stack is empty\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "this indicates a mismatch between enterBlock and quitBlock calls\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "this is a compiler bug, please report it with:\n" ++
        "   " ++ b ++ "=" ++ r ++ "        - the source code that triggered this error\n" ++
        "   " ++ b ++ "=" ++ r ++ "        - context: block stack management error\n"
      fail "error, no block to quit"
    (_:blocks) -> updateState $ stack .~ blocks


--
-- parseU :: Interpret α -> Interpret (α, RTState) 
-- parseU p = do
--     res <- p -- Executa seu parser principal
--     state <- getState -- Pega o estado final
--     pure (res, state)

-- parser :: [Token] -> IO (Either ParseError ([Token], RTState))
-- parser = runParserT (parseU undefined) freshState "Parsing error!" 
--
--
-- | Recursively skips tokens until matching closing brace.
--
-- Tracks brace depth to handle nested blocks.
--
-- @
-- | Switches from 'Running' to 'Analysing' phase.
--
-- Used to enter static analysis mode (skips execution).
turnOff :: Interpreter ()
turnOff = do 
  updateState $
    flag %~ \case 
        Running -> Analysing
        other   -> other

-- | Switches to 'Running' phase.
--
-- Enables full execution (evaluates expressions, runs statements).
turnOn :: Interpreter ()
turnOn = do 
  updateState $
    flag .~ Running

-- | Binds function parameters to argument values.
--
-- Creates local variables for each parameter with corresponding argument value.
--
-- Fails if:
--
-- * Parameter count doesn't match argument count
-- * Argument types don't match parameter types
--
-- In 'Analysing' phase, creates variables without values (type checking only).
--
-- @
-- unifyParams [("x", TInt, False)] [(VInt 42, TInt)]
-- @
unifyParams :: [(Ident, Typε, Bool)] -> [ValueInfo] -> Interpreter ()
unifyParams params args
  | length params /= length args = fail $ "* Wrong number of arguments: expected " 
      ++ show (length params) ++ " but got " ++ show (length args)
  | otherwise = do
    state <- getState
    forM_ (zip params args) $ \((ident, paramType, is_const), (value, argType)) -> do
      -- Verify type compatibility
      unless (argType == paramType) $
        fail $ "Type Error: Function parameter '" ++ ident 
            ++ "' expects type " ++ show paramType 
            ++ " but received " ++ show argType
      
      let mValue = if isRunning state then Just value else Nothing 
      addVar ident paramType mValue is_const
