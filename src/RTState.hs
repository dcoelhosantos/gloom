-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : RTState
Description : Runtime state management for the Gloom interpreter
Copyright   : (c) João, 2024-2025
License     : MIT
Stability   : experimental

This module defines the runtime state ('RTState') used during program execution.
The state tracks variables, functions, types, control flow flags, and heap memory.

= State Components

* __Type System__: Custom struct and variant definitions
* __Variables__: Global variables and stack-based scopes  
* __Functions__: Function signatures and bodies (as token streams)
* __Heap__: Dynamically allocated memory with @new@ and @drop@
* __Control Flow__: Flags for breaks, returns, and execution phase

= Two-Phase Model

The 'Flag' type controls which phase we're in:

* 'Analysing' - Type checking with placeholder values
* 'Running' - Actual execution with real values
* 'InBreak' - Inside a break statement
* 'Returning' - Function is returning a value

= Scoping

Gloom uses lexical scoping with a stack of 'Scope's. Each scope has:

* A unique 'Prefix' (scope ID)
* A symbol table mapping identifiers to 'VarInfo'

Global variables live in 'rtGlobals' (prefix = -1).

== Example

@
-- Creating a new scope for a function
enterBlock :: Interpreter ()
exitBlock :: Interpreter ()

-- Looking up a variable (searches stack then globals)
searchVarInfo :: Ident -> Interpreter (Maybe (Prefix, VarInfo))
@

See @code-examples/01-basics/@ for scoping examples.
-}
module RTState
  ( -- * Core Types
    RTState(..)
  , RTS
  , RunTimeState

    -- * Variable Information
  , VarInfo(..)
  , Prefix
  
    -- * Scope Management
  , Scope(..)

    -- * Execution Flags
  , Flag(..)

    -- * Function Management
  , FunAss
  , CallSignature(..)

    -- * State Initialization
  , freshState

    -- * Lenses for State Access
  , types
  , globals
  , stack
  , funs
  , heap
  , heapAddr
  , rtPrefix
  , refreshPrefix
  , flag
  , returning
  , debit
  , expectedType

    -- * Phase Control
  , isRunning
  , setExpectedType
  , clearExpectedType
  , getExpectedType

    -- * Debit System (Type Inference)
  , addDebit
  , resolveDebits
  , checkUnresolvedDebits
  ) where

import AST
import Lexing.Lexer
import Control.Lens

import qualified Data.Map as Map
import qualified Data.List as List

-- | Variable information stored in scope tables
--
-- Each variable has:
--
-- * 'varValue' - The actual value (Nothing if uninitialized)
-- * 'varType' - The declared type
-- * 'isConst' - Whether it's a constant (immutable after initialization)
-- * 'wasInitialized' - Whether the variable has been assigned a value
--
-- Note: Constants MUST be initialized at declaration!
--
-- The 'wasInitialized' flag is used for compile-time checking to ensure
-- variables are not used before being assigned. This is checked during
-- the Analysing phase to catch uninitialized variable access early.
--
-- == Example
--
-- @
-- var x: int;           -- VarInfo Nothing TInt False False
-- var y: int = 42;      -- VarInfo (Just (VInt 42)) TInt False True
-- const PI: float = 3.14;  -- VarInfo (Just (VFloat 3.14)) TFloat True True
-- x = 10;               -- Now: VarInfo (Just (VInt 10)) TInt False True
-- @
data VarInfo = VarInfo
  { varValue :: Maybe Value  -- ^ Current value (Nothing if uninitialized)
  , varType  :: Typε         -- ^ Declared type
  , isConst  :: Bool         -- ^ Is this a constant?
  , wasInitialized :: Bool   -- ^ Has been assigned a value?
  } deriving (Eq, Show)

-- | Scope prefix - unique identifier for each lexical scope
--
-- Global scope has prefix -1. Local scopes get incrementing positive integers.
type Prefix = Integer

-- | Lexical scope containing variables
--
-- Each function call, block, or control structure creates a new scope.
-- Scopes are organized as a stack in 'RTState'.
--
-- The 'getPrefix' uniquely identifies this scope, allowing variables
-- with the same name in different scopes to coexist.
data Scope = Scope
  { getPrefix :: Prefix                     -- ^ Unique scope identifier
  , getTable  :: Map.Map Ident VarInfo      -- ^ Symbol table for this scope
  } deriving (Eq)

instance Show Scope where
  show (Scope c maps) =
    "Scope: [" ++ show c ++ "]\n"
    ++ prettyMap (Map.toList maps)
    where
      prettyMap :: [(Ident, VarInfo)] -> String
      prettyMap entries = unlines (fmap formatEntry entries)

      formatEntry :: (Ident, VarInfo) -> String
      formatEntry (ident, VarInfo {..}) =
        "  " ++ show ident ++ ": " ++ show varType ++ constStr ++ initStr ++ " = " ++ valueStr
        where 
          constStr = if isConst then " [const]" else ""
          initStr = if wasInitialized then "" else " [uninitialized]"
          valueStr = maybe "<unassigned>" show varValue

-- | Execution phase and control flow flags
--
-- Controls which phase of execution we're in and tracks control flow state.
--
-- = Execution Phases
--
-- * 'Analysing' - Phase 1: Type checking, validation, symbol collection
-- * 'Running' - Phase 2: Actual execution with runtime values
--
-- = Control Flow States
--
-- * 'InBreak' - Currently executing a break statement
-- * 'Returning' - Function is returning (carries the return value and type)
--
-- The two-phase model allows us to catch errors at "compile time" while
-- still being an interpreter. Phase 1 uses placeholder/default values,
-- Phase 2 uses actual runtime values.
data Flag 
  = Running              -- ^ Phase 2: Execute with runtime state
  | Analysing            -- ^ Phase 1: Type check with placeholders
  | InBreak              -- ^ Inside a break statement
  | Returning (Maybe (Value, Typε))  -- ^ Returning from function
  deriving (Eq, Show, Ord)

-- | Function signature: (return type, parameters, optional body tokens)
--
-- Parameters are: (name, type, isReference)
-- The body is stored as tokens to allow re-parsing with updated state.
type FunAss = (Typε, [(Ident, Typε, Bool)], Maybe [Token])

-- | Function call signature - tracks unresolved function calls during analysis
--
-- When a function is called before being declared, we record:
-- * The function name
-- * The call location (for error reporting)
-- * Parameter types used in the call
-- * Expected return type (inferred from usage context)
--
-- This enables mutual recursion and forward references.
-- During analysis, if a function is called but not yet declared, we add it to 'debit'.
-- When the function is later declared, we validate that the signature matches.
-- If it doesn't match, the error points to the CALL site, not the declaration.
data CallSignature = CallSignature
  { callName :: Ident              -- ^ Function name
  , callTokens :: [Token]          -- ^ Tokens for error location
  , callParams :: [Typε]           -- ^ Parameter types from call site
  , callReturnType :: Typε         -- ^ Expected return type from context
  } deriving (Eq, Show)

-- | Alias for 'RTState' (legacy name)
type RunTimeState = RTState

-- | Short alias for 'RTState' (commonly used throughout the codebase)
type RTS = RTState

-- | Complete runtime state for the Gloom interpreter
--
-- This is the central state threaded through all parsing and execution.
-- It contains everything needed to run a Gloom program:
--
-- = Type System
-- * 'rtTypes' - User-defined struct and variant definitions
--
-- = Variables
-- * 'rtGlobals' - Global variables (prefix -1)
-- * 'rtStack' - Stack of local scopes with their variables
--
-- = Functions
-- * 'rtFuns' - Function signatures and bodies
--
-- = Heap Memory
-- * 'rtHeap' - Dynamically allocated values (@new@)
-- * 'nextHeapAddr' - Next available heap address
--
-- = Control Flow
-- * 'rtFlag' - Current execution phase and control state
-- * 'rtReturn' - Expected return type of current function
-- * 'freshPrefix' - Counter for generating unique scope IDs
--
-- = Function Call Tracking
-- * 'rtDebit' - Unresolved function calls during analysis (for mutual recursion)
--
-- == State Evolution
--
-- @
-- Phase 1: freshState {rtFlag = Analysing} 
--   → collect globals, check types
--   → produces finalState1
--
-- Phase 2: finalState1 {rtFlag = Running}
--   → execute with real values
--   → produces final result
-- @
data RTState = RTState
  { rtTypes        :: Map.Map Ident TypeDef         -- ^ User-defined types
  , rtGlobals      :: Map.Map Ident VarInfo         -- ^ Global variables
  , rtStack        :: [(Scope, Bool)]               -- ^ Scope stack (scope, isInLoop)
  , rtFuns         :: Map.Map Ident FunAss          -- ^ Function definitions
  , rtFlag         :: Flag                          -- ^ Current execution state
  , freshPrefix    :: Prefix                        -- ^ Next scope ID
  , rtReturn       :: Maybe Typε                    -- ^ Expected return type
  , rtHeap         :: Map.Map HeapAddr (Value, Typε)  -- ^ Heap-allocated memory
  , nextHeapAddr   :: HeapAddr                      -- ^ Next heap address
  , rtDebit        :: Map.Map Ident [CallSignature] -- ^ Unresolved function calls
  , rtExpectedType :: Maybe Typε                    -- ^ Type expected in current expression context
  } deriving (Eq, Show)

-- | Initial runtime state for starting a Gloom program
--
-- Creates a fresh state ready for Phase 1 (Analysing).
-- Contains one empty scope (scope 0) so variable lookups work properly.
--
-- == Initial Values
--
-- * No types defined
-- * No global variables
-- * One empty local scope (prefix 0)
-- * No functions defined
-- * Flag set to 'Analysing' (Phase 1)
-- * Next scope ID will be 1
-- * No expected return type
-- * Empty heap, next address is 1
--
-- This is the starting point passed to the parser and type checker.
freshState :: RTState
-- Old testing state with sample data (commented out):
-- freshState = RTState testing Map.empty [(Scope 0 testingVar, False)] Map.empty Analysing (succ 0) Nothing Map.empty 1
--   where
--     testing = Map.insert "Car" (DStruct "Car" $ Map.singleton "year" TInt)
--       $ Map.singleton "Test" . DVariant "Test" $ Map.singleton "A" []
--     testingVar = Map.insert "car" car $ Map.singleton "x" $ VarInfo (Just $ VInt 42) TInt False
--     car = VarInfo (Just $ VStruct "Car" (Map.singleton "year" (Just $ VInt 2012, TInt))) (TCustom "Car") False

freshState = RTState 
  { rtTypes = Map.empty
  , rtGlobals = Map.empty
  , rtStack = [(Scope 0 Map.empty, False)]  -- Empty scope instead of completely empty stack
  , rtFuns = Map.empty
  , rtFlag = Analysing
  , freshPrefix = 1
  , rtReturn = Nothing
  , rtHeap = Map.empty
  , nextHeapAddr = 1
  , rtDebit = Map.empty  -- No pending function calls initially
  , rtExpectedType = Nothing  -- No type expectation initially
  }

-- * Lenses for State Access
--
-- These lenses provide clean access to RTState fields using Control.Lens.
-- They're used throughout the parser and runtime for state manipulation.

-- | Lens for accessing user-defined types
types :: Lens' RTS (Map.Map Ident TypeDef)
types = lens rtTypes (\rtState new_types -> rtState { rtTypes = new_types })

-- | Lens for accessing global variables
globals :: Lens' RTS (Map.Map Ident VarInfo)
globals = lens rtGlobals (\rtState new_globals -> rtState { rtGlobals = new_globals} )

-- | Lens for accessing the scope stack
stack :: Lens' RTS [(Scope, Bool)]
stack = lens rtStack (\rtState new_stack -> rtState { rtStack = new_stack })

-- | Lens for accessing function definitions
funs :: Lens' RTS (Map.Map Ident FunAss)
funs = lens rtFuns (\rtState new_Funs -> rtState { rtFuns = new_Funs })

-- | Lens for accessing the heap
heap :: Lens' RTS (Map.Map HeapAddr (Value, Typε))
heap = lens rtHeap (\rtState new_heap -> rtState { rtHeap = new_heap })

-- | Lens for accessing the next heap address counter
heapAddr :: Lens' RTS HeapAddr
heapAddr = lens nextHeapAddr (\rtState new_addr -> rtState { nextHeapAddr = new_addr })

-- | Lens for accessing the scope prefix counter
rtPrefix :: Lens' RTS Prefix
rtPrefix = lens freshPrefix (\rtState new_prefix -> rtState { freshPrefix = new_prefix })

-- | Increment the scope prefix counter (for generating new scope IDs)
refreshPrefix :: RTS -> RTS
refreshPrefix = over rtPrefix succ

-- * Control Flow Helpers

-- | Lens for accessing the execution flag
flag :: Lens' RTS Flag
flag = lens rtFlag (\rtState new_flag -> rtState { rtFlag = new_flag })

-- | Lens for accessing the expected return type
returning :: Lens' RTS (Maybe Typε)
returning = lens rtReturn (\rtState new_return -> rtState { rtReturn = new_return })

-- | Lens for accessing the debit table (unresolved function calls)
debit :: Lens' RTS (Map.Map Ident [CallSignature])
debit = lens rtDebit (\rtState new_debit -> rtState { rtDebit = new_debit })

-- | Lens for accessing the expected type in current expression context
expectedType :: Lens' RTS (Maybe Typε)
expectedType = lens rtExpectedType (\rtState new_expected -> rtState { rtExpectedType = new_expected })

-- | Check if we're in the Running phase (Phase 2)
isRunning :: RTState -> Bool
isRunning = (Running==) . view flag 

-- | Set expected type for current expression context
--
-- Used to propagate type expectations down the expression tree.
-- When parsing an expression in a context that expects a specific type
-- (e.g., return statement, binary operation), we set this so that
-- unresolved function calls can infer their return type.
setExpectedType :: Typε -> RTState -> RTState
setExpectedType t = set expectedType (Just t)

-- | Clear expected type (back to no expectation)
clearExpectedType :: RTState -> RTState
clearExpectedType = set expectedType Nothing

-- | Get the current expected type, if any
getExpectedType :: RTState -> Maybe Typε
getExpectedType = view expectedType


-- * Debit Management (Mutual Recursion Support)

-- | Add a function call to the debit table
--
-- Called during analysis when a function is invoked before being declared.
-- Records the call signature for later validation.
addDebit :: Ident -> CallSignature -> RTState -> RTState
addDebit funName callSig = over debit (Map.insertWith (++) funName [callSig])

-- | Resolve debits for a newly declared function
--
-- When a function is declared, check if there are pending calls to it.
-- Validates that all calls match the declared signature.
-- Returns Nothing if all calls are valid, or Just error message if mismatched.
resolveDebits :: Ident -> Typε -> [Typε] -> RTState -> Either String RTState
resolveDebits funName returnType paramTypes state =
  case Map.lookup funName (view debit state) of
    Nothing -> Right state  -- No pending calls, all good
    Just callSigs -> 
      -- Check each call signature
      case findMismatch callSigs of
        Nothing -> Right $ over debit (Map.delete funName) state  -- All match, remove from debit
        Just (callSig, reason) -> Left $ formatError callSig reason
  where
    findMismatch :: [CallSignature] -> Maybe (CallSignature, String)
    findMismatch [] = Nothing
    findMismatch (sig:rest) =
      if callReturnType sig /= returnType
        then Just (sig, "Return type mismatch: expected " ++ show (callReturnType sig) ++ ", got " ++ show returnType)
      else if List.length (callParams sig) /= List.length paramTypes
        then Just (sig, "Parameter count mismatch: expected " ++ show (List.length $ callParams sig) ++ " params, got " ++ show (List.length paramTypes))
      else if callParams sig /= paramTypes
        then Just (sig, "Parameter type mismatch: expected " ++ show (callParams sig) ++ ", got " ++ show paramTypes)
      else findMismatch rest
    
    formatError :: CallSignature -> String -> String
    formatError callSig reason =
      "Function '" ++ funName ++ "' " ++ reason ++ "\n" ++
      "  Called at: " ++ showTokenLocation (callTokens callSig)
    
    showTokenLocation :: [Token] -> String
    showTokenLocation [] = "<unknown location>"
    showTokenLocation (t:_) = "line " ++ show (tokenLine t) ++ ", col " ++ show (tokenCol t)

-- | Check if there are any unresolved debits
--
-- Should be called before transitioning from Analysing to Running phase.
-- Returns Nothing if all debits resolved, or Just error message listing undeclared functions.
checkUnresolvedDebits :: RTState -> Maybe String
checkUnresolvedDebits state =
  let debits = Map.toList (view debit state)
  in if null debits
    then Nothing
    else Just $ "Undeclared functions:\n" ++ unlines (List.map formatDebit debits)
  where
    formatDebit :: (Ident, [CallSignature]) -> String
    formatDebit (funName, callSigs) =
      "  - '" ++ funName ++ "' called at:\n" ++
      unlines (List.map (\sig -> "      " ++ showTokenLocation (callTokens sig)) callSigs)
    
    showTokenLocation :: [Token] -> String
    showTokenLocation [] = "<unknown location>"
    showTokenLocation (t:_) = "line " ++ show (tokenLine t) ++ ", col " ++ show (tokenCol t)
