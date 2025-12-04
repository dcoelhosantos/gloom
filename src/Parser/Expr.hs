
-- | Expression evaluation operators (binary/unary).
--
-- This module implements all Gloom operators using the 'InterPerator' type
-- (an alias for parsers that return evaluated values and consumed tokens).
--
-- Operators:
--
-- * Unary: @-@, @!@
-- * Arithmetic: @*@, @/@, @%@, @+@, @-@
-- * Comparison: @<=@, @>=@, @<@, @>@, @!=@, @==@
-- * Logical: @&&@, @||@
-- * Ternary: @if-then-else@
-- * Literals: constants, constructors
--
-- All operators perform:
--
-- 1. Type checking (in 'Analysing' phase)
-- 2. Value computation (in 'Running' phase)
module Parser.Expr
  ( -- * Type Aliases
    InterPerator

    -- * Helpers
  , singleField

    -- * Unary Operators
  , eNeg
  , eNot

    -- * Arithmetic Operators
  , eMul
  , eDiv
  , eRem
  , eAdd
  , eSub

    -- * String Operators
  , eConcat

    -- * Comparison Operators
  , eLEQ
  , eGEQ
  , eLT
  , eGT
  , eNEq
  , eEq

    -- * Logical Operators
  , eAnd
  , eOr

    -- * Ternary Operator
  , eIfThenElse

    -- * Literals
  , eLit
  ) where


--global 

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Lexing.Lexer
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Text.Parsec (getState, modifyState)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.List as List

-- local
import AST
import Parser.General
import RTState (isRunning, setExpectedType, clearExpectedType)
import qualified Error.Formatter as ErrFmt

import qualified Lexing.Tokens.Identifiers as Token
import qualified Lexing.Tokens.Literals as Token
import qualified Lexing.Tokens.Symbols as Token

-- | Type alias for expression evaluators.
--
-- Returns:
--
-- * 'ValueInfo': (value, type) of the evaluated expression
-- * @[Token]@: consumed tokens (for tracking)
type InterPerator = Interpreter (ValueInfo, [Token])

-- | Parser for struct field access (@.field@).
--
-- Returns a function that takes an 'InterPerator' (struct expression)
-- and adds field access to it:
--
-- @
-- person.name  -- singleField creates a wrapper around 'person' expression
-- @
--
-- Behavior differs by phase:
--
-- * 'Analysing': uses type information to validate field exists
-- * 'Running': extracts actual field value from struct
singleField :: Interpreter (InterPerator -> InterPerator)
singleField = do
  dot_T <- Token.dot
  (field, field_T) <- Token.varId

  pure $ \interperator -> do
    ((val, typ), struct_tokens) <- interperator
    
    state <- getState
    let shouldRun = isRunning state
    
    -- During analysis, we might only have type information
    -- During execution, we need the actual struct value
    (ident, fields, sIdent) <- case (val, typ, shouldRun) of
      -- Case 1: During execution with actual struct value
      (VStruct ident fields, TCustom sIdent, True) -> 
        pure (ident, fields, sIdent)
      
      -- Case 2: During analysis - either just type or struct with defaults
      (_, TCustom sIdent, False) -> do
        -- Look up struct definition to get field types
        typeDef <- searchStruct sIdent
        case typeDef of
          DStruct _ fieldsMap -> 
            -- Create a map with no values, only types
            pure (sIdent, Map.map (\t -> (Nothing, t)) fieldsMap, sIdent)
      
      -- Case 3: Reference to struct (from dereferencing) - unwrap the reference
      -- This handles (*p).field where p: &Point during ANALYSIS
      (_, TRef (TCustom sIdent), False) -> do
        -- Look up struct definition to get field types
        typeDef <- searchStruct sIdent
        case typeDef of
          DStruct _ fieldsMap -> 
            -- Create a map with no values, only types
            pure (sIdent, Map.map (\t -> (Nothing, t)) fieldsMap, sIdent)
      
      -- Case 4: Reference during execution - dereference inline
      -- This handles the bug where (*p).x doesn't properly dereference during RUNNING
      (VRef (StackRef ident prefix), TRef (TCustom sIdent), True) -> do
        -- Resolve the reference to get the actual struct
        (targetVal, targetType) <- searchVarValueAt prefix ident
        case (targetVal, targetType) of
          (VStruct structIdent fields, TCustom _) -> 
            pure (structIdent, fields, sIdent)
          _ -> fail $ "Internal Error: Expected struct after dereferencing, got " ++ show targetType
      
      (VRef _, TRef (TCustom _), True) -> 
        fail "Internal Error: Complex references (Field/Index) not yet supported in field access"
      
      _ -> fail $ "Type Error: Cannot access field of non-struct type: " ++ show typ
    
    unless (ident == sIdent) $ fail "Internal Error: Value identifier does not match type identifier."
    void $ searchStruct sIdent
    
    case Map.lookup field fields of
      Nothing -> do
        let line = tokenLine field_T
            col = tokenCol field_T
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
            availableFields = Map.keys fields
        
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0609]: no field '" ++ field ++ "' on type '" ++ sIdent ++ "'" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " struct." ++ field ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "       " ++ replicate (length field) '^' ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "field '" ++ field ++ "' does not exist in struct '" ++ sIdent ++ "'\n" ++
          (if null availableFields 
           then ""
           else "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "available fields: " ++ 
                c ++ List.intercalate ", " availableFields ++ r ++ "\n") ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "check the field name spelling\n"
        
        fail $ "Name Error: FieldRef '" ++ field ++ "' was not found in struct '" ++ sIdent ++ "'."
      Just (mVal, fieldT) -> do
        fieldVal <- case shouldRun of
          True -> maybe (defaultValue fieldT) pure mVal  -- Execution: need actual value
          False -> pure (placeholderValue fieldT)  -- Analysis: type-appropriate placeholder
        pure ((fieldVal, fieldT), tokens) where
          tokens = struct_tokens ++ [dot_T] ++ [field_T]


-- | Unary negation operator (@-@).
--
-- Supports: @TInt@, @TFloat@
--
-- @
-- eNeg minusToken (interperatorFor "42")
-- -- Evaluates to: (-42, TInt)
-- @
eNeg :: Token -> InterPerator -> InterPerator
eNeg token iE = do
  ((v, t), num_Ts) <- iE

  newValueInfo <- case (v, t) of
    (VInt n, TInt) ->
      pure (VInt $ -n , TInt)
    (VFloat x, TFloat) ->
      pure (VFloat $ -x , TFloat)
    (_, invalidT) -> do
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in unary negation" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " -value\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "unary negation operator '-' expects Int or Float\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ c ++ show invalidT ++ r ++ " instead\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert the value to Int or Float first\n"
      fail $ "Type Error: Unary negation '-' expects Int or Float, but got " ++ show invalidT

  let tokens = token:num_Ts
  pure (newValueInfo, tokens)

-- | Logical NOT operator (@!@).
--
-- Supports: @TBool@
--
-- @
-- eNot notToken (interperatorFor "true")
-- -- Evaluates to: (false, TBool)
-- @
eNot :: Token -> InterPerator -> InterPerator
eNot token iE = do
  ((val, type_), b_Ts) <- iE

  case (val, type_) of
    (VBool b, TBool) ->
      pure ((VBool $ not b, TBool), token:b_Ts)
    (_, t) -> do
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in logical NOT" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " !value\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "logical NOT operator '!' expects Bool\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ c ++ show t ++ r ++ " instead\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "use a boolean expression or convert to Bool\n"
      fail $ "Type Error: Operator '!' expects Bool, but got " ++ show t

-- | Multiplication operator (@*@). Supports: @TInt@, @TFloat@
eMul :: Token -> InterPerator -> InterPerator -> InterPerator
eMul token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR
  
  -- Reject references in arithmetic operations
  let isRefType (TRef _) = True
      isRefType _ = False
  
  when (isRefType tL || isRefType tR) $ fail $ fopTypeMismatch tL tR "mul"
  
  newValueInfo <- case ((vL, tL), (vR, tR)) of
    ((VInt nL, TInt), (VInt nR, TInt)) ->
      pure (VInt $ nL * nR, TInt)
    ((VFloat xL, TFloat), (VFloat xR, TFloat)) ->
      pure (VFloat $ xL * xR, TFloat)
    -- Allow TAny (forward reference) to unify with the other operand
    ((_, TAny), (_, t)) -> pure (VInt 0, t)
    ((_, t), (_, TAny)) -> pure (VInt 0, t)
    ((_, tL'), (_, tR')) -> fail $ fopTypeMismatch tL' tR' "mul"

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure (newValueInfo, tokens)

-- | Division operator (@/@). Supports: @TInt@ (quot), @TFloat@. Fails on division by zero (runtime only).
eDiv :: Token -> InterPerator -> InterPerator -> InterPerator
eDiv token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR
  
  -- Reject references in arithmetic operations
  let isRefType (TRef _) = True
      isRefType _ = False
  
  when (isRefType tL || isRefType tR) $ fail $ fopTypeMismatch tL tR "div"
  
  state <- getState

  newValueInfo <- case ((vL, tL), (vR, tR)) of
    ((VInt _, _), (VInt 0, _)) -> do
      -- Only check division by zero in Running phase
      when (isRunning state) $ do
        let line = tokenLine token
            col = tokenCol token
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
            
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E1001]: division by zero" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " ... / 0\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "    ^^^" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "integer division by zero causes a runtime panic\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "add a check before dividing: " ++ 
          "if (divisor != 0) { ... }\n"
        
        fail "Runtime Error: Integer division by zero."
      pure (VInt 0, TInt)  -- In Analysing, just return dummy value for type checking
    ((VInt nL, TInt), (VInt nR, TInt)) ->
      pure (VInt $ nL `quot` nR, TInt)
    ((VFloat _, _), (VFloat 0.0, _)) -> do
      when (isRunning state) $ do
        let line = tokenLine token
            col = tokenCol token
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
            
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E1001]: division by zero" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " ... / 0.0\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "    ^^^^^" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "float division by zero causes a runtime panic\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "add a check before dividing: " ++ 
          "if (divisor != 0.0) { ... }\n"
        
        fail "Runtime Error: Float division by zero."
      pure (VFloat 0.0, TFloat)  -- In Analysing, dummy value
    ((VFloat xL, TFloat), (VFloat xR, TFloat)) ->
      pure (VFloat $ xL / xR, TFloat)
    -- Allow TAny (forward reference) to unify with the other operand
    ((_, TAny), (_, t)) -> pure (VInt 0, t)
    ((_, t), (_, TAny)) -> pure (VInt 0, t)
    ((_, tL'), (_, tR')) -> fail $ fopTypeMismatch tL' tR' "div"

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure (newValueInfo, tokens)

-- | Remainder operator (@%@). Supports: @TInt@. Fails on modulo by zero (runtime only).
eRem :: Token -> InterPerator -> InterPerator -> InterPerator
eRem token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR
  
  -- Reject references in arithmetic operations
  let isRefType (TRef _) = True
      isRefType _ = False
  
  when (isRefType tL || isRefType tR) $ fail $ fopTypeMismatch tL tR "rem"
  
  state <- getState

  newValueInfo <- case ((vL, tL), (vR, tR)) of
    ((VInt _, _), (VInt 0, _)) -> do
      -- Only check modulo by zero in Running phase
      when (isRunning state) $ do
        let line = tokenLine token
            col = tokenCol token
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
            
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E1001]: remainder by zero" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " ... % 0\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "    ^^^" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "remainder by zero causes a runtime panic\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "add a check before taking remainder: " ++ 
          "if (divisor != 0) { ... }\n"
        
        fail "Runtime Error: Remainder by zero."
      pure (VInt 0, TInt)  -- In Analysing, dummy value
    ((VInt nL, TInt), (VInt nR, TInt)) ->
      pure (VInt $ nL `rem` nR, TInt)
    ((_, tL'), (_, tR')) -> fail $ fopTypeMismatch tL' tR' "rem"

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure (newValueInfo, tokens)

-- | Addition operator (@+@). Supports: @TInt@, @TFloat@
eAdd :: Token -> InterPerator -> InterPerator -> InterPerator
eAdd token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  
  -- After evaluating left operand, we know what type to expect on the right
  -- Set expected type for right operand (unless left is TAny)
  when (tL /= TAny) $ modifyState $ setExpectedType tL
  
  ((vR, tR), iR_Ts) <- iR
  
  -- Clear expected type after evaluation
  modifyState clearExpectedType

  -- Reject references in arithmetic operations
  let isRefType (TRef _) = True
      isRefType _ = False
  
  when (isRefType tL || isRefType tR) $ fail $ fopTypeMismatch tL tR "add"

  newValueInfo <- case ((vL, tL), (vR, tR)) of
    ((VInt nL, TInt), (VInt nR, TInt)) ->
      pure (VInt $ nL + nR, TInt)
    ((VFloat xL, TFloat), (VFloat xR, TFloat)) ->
      pure (VFloat $ xL + xR, TFloat)
    -- Allow TAny (forward reference) to unify with the other operand
    ((_, TAny), (_, t)) -> pure (VInt 0, t)  -- Placeholder during analysis
    ((_, t), (_, TAny)) -> pure (VInt 0, t)  -- Placeholder during analysis
    ((_, tL'), (_, tR')) -> fail $ fopTypeMismatch tL' tR' "add"

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure (newValueInfo, tokens)

-- | Array concatenation operator (@<>@). Returns new array (functional style).
--
-- Concatenates two arrays: @[1, 2] <> [3, 4]@ → @[1, 2, 3, 4]@
eConcat :: Token -> InterPerator -> InterPerator -> InterPerator
eConcat token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR

  -- Type check: both must be dynamic arrays
  newValueInfo <- case (tL, tR) of
    (TDynArray elemTL, TDynArray elemTR) -> do
      -- Element types must match
      unless (elemTL == elemTR) $ fail $
        "Type Error: Cannot concatenate arrays of different element types: "
        ++ show elemTL ++ " <> " ++ show elemTR
      
      -- Runtime: concatenate the vectors
      case (vL, vR) of
        (VDynArray arrL, VDynArray arrR) ->
          pure (VDynArray $ arrL Vector.++ arrR, TDynArray elemTL)
        _ -> pure (vL, tL)  -- During analysis, just return type info
    
    (_, _) -> fail $
      "Type Error: Operator '<>' expects two arrays, but got "
      ++ show tL ++ " <> " ++ show tR

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure (newValueInfo, tokens)

-- | Subtraction operator (@-@). Supports: @TInt@, @TFloat@
eSub :: Token -> InterPerator -> InterPerator -> InterPerator
eSub token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR

  -- Reject references in arithmetic operations
  let isRefType (TRef _) = True
      isRefType _ = False
  
  when (isRefType tL || isRefType tR) $ fail $ fopTypeMismatch tL tR "sub"

  newValueInfo <- case ((vL, tL), (vR, tR)) of
    ((VInt nL, TInt), (VInt nR, TInt)) ->
      pure (VInt $ nL - nR, TInt)
    ((VFloat xL, TFloat), (VFloat xR, TFloat)) ->
      pure (VFloat $ xL - xR, TFloat)
    ((_, tL'), (_, tR')) -> fail $ fopTypeMismatch tL' tR' "sub"

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure (newValueInfo, tokens)


-- | Less-than-or-equal operator (@<=@). Supports: @TInt@, @TFloat@, @TText@
eLEQ :: Token -> InterPerator -> InterPerator -> InterPerator
eLEQ token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR

  unless (tL == tR) . fail $
    "Type Error: Cannot compare different types " ++ show tL ++ " <= " ++ show tR
  
  case tL of
    TInt    -> pure()
    TFloat  -> pure()
    TText   -> pure()
    _       -> do
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in comparison" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " left <= right\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^" ++ r ++ "    " ++ y ++ "^^^^^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ c ++ " " ++ show tL ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "operator '<=' expects numeric or text types\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "valid types: Int, Float, Text\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ c ++ show tL ++ r ++ " instead\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert to a comparable type or use a different operator\n"
      fail $ "Type Error: Operator '<=' expects numeric or text types (Int/Float/Text), but got " ++ show tL

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure ((VBool $ vL <= vR, TBool), tokens)

-- | Greater-than-or-equal operator (@>=@). Supports: @TInt@, @TFloat@, @TText@
eGEQ :: Token -> InterPerator -> InterPerator -> InterPerator
eGEQ token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR

  unless (tL == tR) . fail $
    "Type Error: Cannot compare different types " ++ show tL ++ " >= " ++ show tR

  case tL of
    TInt    -> pure()
    TFloat  -> pure()
    TText   -> pure()
    _       -> do
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in comparison" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " left >= right\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^" ++ r ++ "    " ++ y ++ "^^^^^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ c ++ " " ++ show tL ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "operator '>=' expects numeric or text types\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "valid types: Int, Float, Text\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ c ++ show tL ++ r ++ " instead\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert to a comparable type or use a different operator\n"
      fail $ "Type Error: Operator '>=' expects numeric or text types (Int/Float/Text), but got " ++ show tL

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure ((VBool $ vL >= vR, TBool), tokens)

-- | Less-than operator (@<@). Supports: @TInt@, @TFloat@, @TText@
eLT :: Token -> InterPerator -> InterPerator -> InterPerator
eLT token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR

  unless (tL == tR) . fail $
    "Type Error: Cannot compare different types " ++ show tL ++ " < " ++ show tR

  case tL of
    TInt    -> pure()
    TFloat  -> pure()
    TText   -> pure()
    _       -> do
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in comparison" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " left < right\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^" ++ r ++ "   " ++ y ++ "^^^^^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ c ++ " " ++ show tL ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "operator '<' expects numeric or text types\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "valid types: Int, Float, Text\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ c ++ show tL ++ r ++ " instead\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert to a comparable type or use a different operator\n"
      fail $ "Type Error: Operator '<' expects numeric or text types (Int/Float/Text), but got " ++ show tL

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure ((VBool $ vL < vR, TBool), tokens)

-- | Greater-than operator (@>@). Supports: @TInt@, @TFloat@, @TText@
eGT :: Token -> InterPerator -> InterPerator -> InterPerator
eGT token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR

  unless (tL == tR) . fail $
    "Type Error: Cannot compare different types " ++ show tL ++ " > " ++ show tR

  case tL of
    TInt    -> pure()
    TFloat  -> pure()
    TText   -> pure()
    _       -> do
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in comparison" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " left > right\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^" ++ r ++ "   " ++ y ++ "^^^^^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ c ++ " " ++ show tL ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "operator '>' expects numeric or text types\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "valid types: Int, Float, Text\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ c ++ show tL ++ r ++ " instead\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert to a comparable type or use a different operator\n"
      fail $ "Type Error: Operator '>' expects numeric or text types (Int/Float/Text), but got " ++ show tL

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure ((VBool $ vL > vR, TBool), tokens)

-- | Not-equal operator (@!=@). Supports all types (requires matching types)
eNEq :: Token -> InterPerator -> InterPerator -> InterPerator
eNEq token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR
  unless (tL == tR) . fail $
    "Type Error: Cannot compare different types " ++ show tL ++ " != " ++ show tR

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure ((VBool $ vL /= vR, TBool), tokens)

-- | Equality operator (@==@). Supports all types (requires matching types)
eEq :: Token -> InterPerator -> InterPerator -> InterPerator
eEq token iL iR = do
  ((vL, tL), iL_Ts) <- iL
  ((vR, tR), iR_Ts) <- iR
  unless (tL == tR) . fail $
    "Type Error: Cannot compare different types " ++ show tL ++ " == " ++ show tR

  let tokens = iL_Ts ++ [token] ++ iR_Ts
  pure ((VBool $ vL == vR, TBool), tokens)

-- | Logical AND operator (@&&@). Supports: @TBool@
eAnd :: Token -> InterPerator -> InterPerator -> InterPerator
eAnd token iBL iBR = do
  ((valL, typeL), bL_Ts) <- iBL
  ((valR, typeR), bR_Ts) <- iBR

  case ((valL, typeL), (valR, typeR)) of
    ((VBool bL, TBool), (VBool bR, TBool)) ->
      pure ((VBool $ bL && bR, TBool), bL_Ts ++ [token] ++ bR_Ts)
    _ -> do
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in logical AND" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " left && right\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^" ++ r ++ "    " ++ y ++ "^^^^^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ c ++ " " ++ show typeL ++ replicate (max 0 (4 - length (show typeL))) ' ' ++ "    " ++ show typeR ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "operator '&&' requires both operands to be of type Bool\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found: " ++ c ++ show typeL ++ r ++ " && " ++ c ++ show typeR ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert both operands to Bool or use a different operator\n"
      fail $ "Type Error: Operator '&&' requires both operands to be Bool. Got " ++ show typeL ++ " and " ++ show typeR

-- | Logical OR operator (@||@). Supports: @TBool@
eOr :: Token -> InterPerator -> InterPerator -> InterPerator
eOr token iBL iBR = do
  ((valL, typeL), bL_Ts) <- iBL
  ((valR, typeR), bR_Ts) <- iBR

  case ((valL, typeL), (valR, typeR)) of
    ((VBool bL, TBool), (VBool bR, TBool)) ->
      pure ((VBool $ bL || bR, TBool), bL_Ts ++ [token] ++ bR_Ts)
    _ -> do
      let line = tokenLine token
          col = tokenCol token
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in logical OR" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " left || right\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^" ++ r ++ "    " ++ y ++ "^^^^^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ c ++ " " ++ show typeL ++ replicate (max 0 (4 - length (show typeL))) ' ' ++ "    " ++ show typeR ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "operator '||' requires both operands to be of type Bool\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found: " ++ c ++ show typeL ++ r ++ " || " ++ c ++ show typeR ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert both operands to Bool or use a different operator\n"
      fail $ "Type Error: Operator '||' requires both operands to be Bool. Got " ++ show typeL ++ " and " ++ show typeR

-- | Ternary conditional operator (@?:@).
--
-- Returns a curried parser builder:
--
-- @
-- builder <- eIfThenElse         -- parses '?'
-- combiner <- builder            -- parses ':'
-- result <- combiner cond ifTrue ifFalse
-- @
--
-- Type checks that:
--
-- * Condition is @TBool@
-- * Both branches have the same type
--
-- Evaluates the appropriate branch based on the condition.
eIfThenElse :: Interpreter
              (Interpreter
                 (Interpreter (ValueInfo, [Token])
                  -> Interpreter (ValueInfo, [Token])
                  -> Interpreter (ValueInfo, [Token])
                  -> Interpreter (ValueInfo, [Token])))
eIfThenElse = do
  qMarkToken <- Token.questionMark

  pure $ do
    colonToken <- Token.colon

    pure $ \iCond iLeft iRight -> do
      ((vCond, tCond), cond_Ts) <- iCond

      cond <- case (vCond, tCond) of
        (VBool b, TBool) -> pure b
        (_, t) -> do
          let y = Text.unpack ErrFmt.yellow
              b = Text.unpack ErrFmt.blue
              c = Text.unpack ErrFmt.cyan
              bo = Text.unpack ErrFmt.bold
              r = Text.unpack ErrFmt.reset
          liftIO $ TIO.putStrLn $ Text.pack $
            "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in ternary condition" ++ r ++ "\n" ++
            "  " ++ b ++ "--> " ++ r ++ "<source>\n" ++
            "   " ++ b ++ "|" ++ r ++ "\n" ++
            "   " ++ b ++ "|" ++ r ++ " condition ? if_true : if_false\n" ++
            "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^^^^^^" ++ r ++ " " ++ c ++ "type: " ++ show t ++ r ++ "\n" ++
            "   " ++ b ++ "|" ++ r ++ "\n" ++
            "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "ternary operator '?:' requires condition to be Bool\n" ++
            "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ c ++ show t ++ r ++ " instead\n" ++
            "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert the condition to Bool or use if-else statement\n"
          fail $ "Type Error: 'If' condition must be of type Bool, but got " ++ show t

      ((vL, tL), left_Ts)  <- iLeft
      ((vR, tR), right_Ts) <- iRight

      unless (tL == tR) $ do
        let y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in ternary branches" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ " cond ? if_true : if_false\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "       ^^^^^^^" ++ r ++ "   " ++ y ++ "^^^^^^^^" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ c ++ "       " ++ show tL ++ replicate (max 0 (7 - length (show tL))) ' ' ++ "   " ++ show tR ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "both branches of ternary operator must return the same type\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found: " ++ c ++ show tL ++ r ++ " and " ++ c ++ show tR ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert one branch to match the other type\n"
        fail $ "Type Error: 'If-Then-Else' branches return different types: " ++ show tL ++ " and " ++ show tR

      let tokens
            = cond_Ts ++
            [qMarkToken] ++ left_Ts ++
            [colonToken] ++ right_Ts

      let res = if cond then vL else vR
      pure ((res, tL), tokens)


-- | Parses a literal value (int, float, text, bool, null).
--
-- Returns the literal's runtime value and type.
--
-- @
-- eLit  -- parses "42" -> ((VInt 42, TInt), [token])
-- @
eLit :: Interpreter (ValueInfo, [Token])
eLit = do
  (info, token) <- Token.literal
  pure (info, [token])







