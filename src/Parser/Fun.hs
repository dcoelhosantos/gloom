{-# LANGUAGE RecordWildCards #-}

-- | Core expression and statement parsing (the heart of the Gloom interpreter).
--
-- This module implements:
--
-- * **Expression parsing**: atoms, operators, function calls, struct construction
-- * **Statement parsing**: declarations, assignments, control flow (@if@, @while@, @for@)
-- * **Function definitions**: parameter binding, body evaluation, return handling
-- * **Reference operations**: @&@ (address-of), @*@ (dereference), assignments through pointers
-- * **Heap allocation**: @new@ expressions for dynamic memory
--
-- Most parsing happens in two phases:
--
-- * 'Analysing': type checking, variable resolution (skip function bodies)
-- * 'Running': full evaluation with runtime values
--
-- Key functions:
--
-- * 'expr': main expression parser (uses operator precedence via 'makeExprParser')
-- * 'stmt': main statement parser
-- * 'pFun': function definition parser
-- * 'evalFun': function call evaluator

module Parser.Fun
  ( -- * Main Parsers
    pFun
  , stmt
  , expr
    
    -- * Function Evaluation
  , evalFun
  , evalBodyInTs
  , evalBLockInTs
  , exprInTs
    
    -- * Statements
  , sPrint
  , sPrintf
  , sAbort
  , sIfThenElse
  , sWhile
  , sForEach
  , sReturn
  , sFunCall
  , sIdentBasedStmt
  , sDerefAtrib
  , sDeclVarInit
  , sDeclConstInit
  , sAtrib
  , sDrop
    
    -- * Global Declarations
  , gDeclVar
  , gDeclConst
    
    -- * Array Operations
  , sArrayCons
  , sArrayPush
  , sArrayDrop
  , sArrayUncons
    
    -- * Expressions
  , eNew
  , eInt2Float
  , eFloat2Int
  , eScan
  , eIdentBased
    
    -- * Helpers
  , readT
  , doWhile
  , handleReturns
  , loadBlock
  , evalBlock
  , evalBlockInOff
  ) where


--global 
import qualified Data.Text.IO as TIO   
import qualified Data.Text.Read as TR 
import qualified System.IO as IO
import qualified System.Info as SysInfo
import qualified System.Exit as Exit
import qualified System.Console.Haskeline as Haskeline
import qualified Error.Formatter as ErrFmt


import Control.Lens hiding (index, indices)
import Text.Parsec hiding (token, tokens, label, optional, sourceLine)

import Control.Applicative hiding ((<|>), many)
import Control.Monad.Combinators.Expr
import Control.Monad
import Control.Monad.Trans
import qualified Data.Maybe as Maybe
import qualified Data.Vector as Vector
import Lexing.Lexer (Token(..), TokenType(..), prettyToken)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.List as String

-- local
import AST
import Parser.General

import qualified Lexing.Tokens.Identifiers as Token
import qualified Lexing.Tokens.Symbols as Token
import qualified Lexing.Tokens.Keywords as Token
import RTState

import Parser.Expr
import Parser.Stmt


-- | Generates 15 random emojis for warnings.
-- ========================================
-- Error Formatting Helpers
-- ========================================

-- | Throw a professional type error with source location
typeError :: Token -> Typε -> Typε -> Maybe String -> Interpreter a
typeError tok expected got suggestion = do
  let filepath = "<source>"  -- TODO: Get from RTState
      line = tokenLine tok
      col = tokenCol tok
      sourceLine = Text.pack $ prettyToken tok  -- Show user-friendly token instead of constructor
      expectedText = Text.pack $ show expected
      gotText = Text.pack $ show got
      suggestionText = maybe Text.empty Text.pack suggestion
      errorMsg = ErrFmt.formatTypeError filepath line col sourceLine expectedText gotText suggestionText
  
  liftIO $ TIO.hPutStrLn IO.stderr errorMsg
  fail "Type error occurred"

-- | Throw a professional runtime error with source location
runtimeError :: Token -> String -> Maybe String -> Interpreter a
runtimeError tok msg details = do
  let filepath = "<source>"  -- TODO: Get from RTState
      line = tokenLine tok
      col = tokenCol tok
      sourceLine = Text.pack $ prettyToken tok  -- Show user-friendly token instead of constructor
      errorMsg = ErrFmt.formatRuntimeError filepath line col sourceLine (Text.pack msg) (maybe Text.empty Text.pack details)
  
  liftIO $ TIO.hPutStrLn IO.stderr errorMsg
  fail "Runtime error occurred"

-- | Throw a professional compilation error
compileError :: Token -> String -> String -> Interpreter a
compileError tok errorType msg = do
  let filepath = "<source>"  -- TODO: Get from RTState
      line = tokenLine tok
      col = tokenCol tok
      sourceLine = Text.pack $ prettyToken tok  -- Show user-friendly token instead of constructor
      errorMsg = ErrFmt.formatCompileError filepath line col sourceLine (Text.pack errorType) (Text.pack msg)
  
  liftIO $ TIO.hPutStrLn IO.stderr errorMsg
  fail "Compilation error occurred"


-- | Parses atomic expressions (literals, arrays, identifiers, function calls).
--
-- Tries parsers in priority order:
--
-- 1. Literals (@eLit@)
-- 2. Dynamic arrays (@eDynArray@)
-- 3. References (@eRef@ - @&x@)
-- 4. Heap allocation (@eNew@ - @new Person{...}@)
-- 5. Input (@eScan@)
-- 6. Null checking (@eIsNull@ - @is_null(ptr)@)
-- 7. Identifier-based (variables, function calls, struct access)
atom :: Interpreter (ValueInfo, [Token])
atom = choice
    [ eLit
    , eDynArray
    , try eRef
    , try eNew      -- Heap allocation
    , try eInt2Float  -- Type conversion: int -> float
    , try eFloat2Int  -- Type conversion: float -> int
    , try eIsNull   -- Null pointer check
    , eScan
    -- , try eComp
    , eIdentBased
    --, eFunCall ;
    --, eVar
    ]

-- | Main expression parser (delegates to operator precedence parser).
--
-- Unwraps the monadic layer from 'exprParser'.
expr :: Interpreter (ValueInfo, [Token])
expr = join exprParser

-- | Builds expression parser using operator precedence table.
--
-- Returns a monadic parser (allows operator parsers to access state).
exprParser :: Interpreter (Interpreter (ValueInfo, [Token]))
exprParser = makeExprParser term operatorTable

-- | Parses term-level expressions (atoms with optional field/index access).
--
-- Supports chaining:
--
-- @
-- person.address.city[0]  -- field access + array indexing
-- @
term :: Interpreter (Interpreter (ValueInfo, [Token]))
term = choice
  [ 
    -- Manually handle parentheses to preserve tokens
    do
      openParen_T <- Token.openParens
      innerExpr <- exprParser
      closeParen_T <- Token.closeParens
      pure $ do
        ((val, typ), inner_Ts) <- innerExpr
        pure ((val, typ), openParen_T : inner_Ts ++ [closeParen_T])
  , pure <$> atom
  ]

-- | Parser for array indexing (@[n]@).
--
-- Returns a function that wraps an array expression with index access:
--
-- @
-- arr[2]  -- singleIndex creates a wrapper around 'arr' expression
-- @
--
-- Validates index bounds at runtime.
singleIndex :: Interpreter (InterPerator -> InterPerator)
singleIndex = do
  openBracket_T  <- Token.openBracket
  ((VInt n, TInt), int_Ts) <- expr
  closeBracket_T <- Token.closeBracket

  pure $ \interperator -> do
    ((VDynArray xs, TDynArray arrayType), array_tokens) <- interperator
    
    state <- getState
    -- In Analysing phase, arrays are empty placeholders
    -- Only validate bounds in Running phase
    value <- if isRunning state then
      case xs Vector.!? fromInteger n of
        Nothing -> fail $
          "not a valid index found: " ++ show n
          ++ ". The vector size was: "
          ++ show (Vector.length xs)
        Just x -> pure x
    else
      -- In Analysing, return a placeholder value of the right type
      defaultValue arrayType
    
    let tokens = array_tokens ++ [openBracket_T] ++ int_Ts ++ [closeBracket_T]
    pure ((value, arrayType), tokens)
-- | Operator precedence table for 'makeExprParser'.
--
-- Precedence levels (highest to lowest):
--
-- 1. Postfix: field access (@.@), array indexing (@[]@)
-- 2. Unary prefix: negation (@-@), NOT (@!@), dereference (@*@)
-- 3. Multiplicative: @*@, @\/@, @%@
-- 4. Additive: @+@, @-@
-- 5. Relational: @<=@, @>=@, @<@, @>@
-- 6. Equality: @==@, @!=@
-- 7. Logical AND: @&&@
-- 8. Logical OR: @||@
-- 9. Ternary: @?:@
operatorTable :: [[Operator (ParsecT [Token] RTState IO) (Interpreter (ValueInfo, [Token]))]]
operatorTable =
  [ 
    [ Postfix $ foldr1 (flip (.)) <$> some (singleIndex <|> try singleField)
    ]

  , -- Level 5: Unary prefix
    [ Prefix $ eNeg <$> Token.sub
    , Prefix $ eNot <$> Token.not
    , Prefix $ eDeref <$> Token.mul
    ]

  , -- Level 4: Multiplicative
    [ InfixL $ eMul <$> Token.mul
    , InfixL $ eDiv <$> Token.div
    , InfixL $ eRem <$> Token.rem
    ]

  , -- Level 3: Additive
    [ InfixL $ eAdd <$> Token.add
    , InfixL $ eSub <$> Token.sub
    , InfixL $ eConcat <$> Token.arrayConcat  -- Array concatenation <>
    ]

  , -- Level 2: Relational
    [ InfixN $ eLEQ <$> Token.lesserEquals
    , InfixN $ eGEQ <$> Token.greaterEquals
    , InfixN $ eLT  <$> Token.lesser
    , InfixN $ eGT  <$> Token.greater
    ]
  --
  , -- Level 1: Equality
    [ InfixN $ eNEq <$> Token.notEquals
    , InfixN $ eEq  <$> Token.equals
    ]
  --
  , -- Level 0: Logical AND (higher than OR)
    [ InfixL $ eAnd <$> Token.and ]
  --
  , -- Level -1: Logical OR (lowest (`EBinOp` precedence)
    [ InfixL $ eOr <$> Token.or ]
  --
  , -- Level -2: Ternary ? : (right-associative)

    [ TernR eIfThenElse ]
  ]

-- | Dynamic array literal (@[1, 2, 3]@).
eDynArray :: Interpreter (ValueInfo, [Token])

-- | Identifier-based expression (variable or function call).
--
-- Disambiguates based on next token:
--
-- * @(@: function call
-- * Otherwise: variable reference
eIdentBased :: Interpreter (ValueInfo, [Token])
eIdentBased = do
  (ident, ident_T) <- Token.varId

  choice
    [ finishFunCallExpr ident ident_T =<< Token.openParens 
    , finishVarExpr ident ident_T
    ]

-- | Completes variable reference (looks up value in scope).
finishVarExpr :: Ident -> Token -> Interpreter (ValueInfo, [Token]) 
finishVarExpr vIdent token = do
  value_info <- searchVarValueWithToken vIdent token

  pure (value_info, [token])

-- | Completes function call expression (parses arguments and evaluates).
finishFunCallExpr :: Ident -> Token -> Token -> Interpreter (ValueInfo, [Token])
finishFunCallExpr fun_ident fun_ident_T openParens_T  = do

  (args, args_Ts) <- (`sepBy'` Token.comma) expr
  closeParens_T   <- Token.closeParens
  let tokens = [fun_ident_T, openParens_T] ++ args_Ts ++ [closeParens_T] 

  fun_value <- do 
    mFun <- searchFun fun_ident
    
    case mFun of
      Nothing -> do
        -- Function not found yet - this might be a forward reference
        -- Get the expected return type from context (e.g., in "n + f(x)", we expect int)
        state <- getState
        let inferredReturnType = case getExpectedType state of
              Just t  -> t      -- Use expected type from context
              Nothing -> TAny   -- Fall back to TAny if no context
        
        -- Add a debit for later resolution when the function is declared
        let paramTypes = fmap snd args
        let callSig = CallSignature 
              { callName = fun_ident
              , callTokens = tokens
              , callParams = paramTypes
              , callReturnType = inferredReturnType
              }
        modifyState $ addDebit fun_ident callSig
        
        -- Return placeholder value during Analysing phase
        -- Use the inferred type so type checking can continue
        defaultVal <- defaultValue inferredReturnType
        pure (defaultVal, inferredReturnType)
        
      Just (return_type, fun_params, _) -> do
        finishResolvedFunCall fun_ident return_type fun_params args
        
  pure (fun_value, tokens)

-- | Finishes a resolved function call after type checking and argument matching.
--
-- This is a helper function that completes the execution of a function call
-- once the function signature has been resolved and arguments have been validated.
-- It handles both the 'Analysing' and 'Running' phases differently:
--
-- * In 'Analysing' phase: Creates temporary scope, unifies parameter types with
--   arguments, and returns a default value for the declared return type.
--
-- * In 'Running' phase: Actually evaluates the function with the provided arguments
--   using 'evalFun' and returns the computed result.
--
-- ==== __Parameters__
--
-- [@fun_ident@] The name/identifier of the function being called
-- [@return_type@] The declared return type of the function
-- [@fun_params@] List of (param_name, param_type, is_const) tuples from function signature
-- [@args@] List of (arg_value, arg_type) tuples from the call site
--
-- ==== __Examples__
--
-- @
-- -- During analysis phase:
-- finishResolvedFunCall "add" TInt [("a", TInt, False), ("b", TInt, False)] [(VInt 0, TInt), (VInt 0, TInt)]
-- -- Returns: (VInt 0, TInt) -- default value for TInt
--
-- -- During execution phase:
-- finishResolvedFunCall "add" TInt [...] [(VInt 5, TInt), (VInt 3, TInt)]
-- -- Returns: (VInt 8, TInt) -- actual computed result
-- @
--
-- ==== __Phase Behavior__
--
-- * __Analysing__: Type checks parameters, returns placeholder value
-- * __Running__: Executes function body, returns actual value
--
-- ==== __Side Effects__
--
-- During 'Analysing' phase:
--
-- * Temporarily enters a new scope with 'enterBlock'
-- * Unifies parameter types with arguments via 'unifyParams'
-- * Exits scope with 'quitBlock' (note: uses @quitBlock@ not @exitBlock@)
--
-- During 'Running' phase:
--
-- * Evaluates the function, which may modify heap, globals, or other state
--
-- ==== __Note__
--
-- The function used to warn about void functions (TUnit) being used in expressions,
-- but this check is now commented out because functions with an explicit @return@
-- statement should be allowed without warnings.
--
-- @since 0.1.0
finishResolvedFunCall :: Ident -> Typε -> [(Ident, Typε, Bool)] -> [(Value, Typε)] -> Interpreter (Value, Typε)
finishResolvedFunCall fun_ident return_type fun_params args = do
    
    -- Warn if trying to use void function in expression
    -- COMMENTED: Functions with return @ should be allowed without warning
    -- when (return_type == TUnit) $ do
    --   emojis <- liftIO emogen
    --   liftIO $ putStrLn $ "⚠️  WARNING " ++ emojis ++ " ⚠️"
    --   liftIO $ putStrLn $ "Hey! 1berto não gostou! " 
    --     ++ show fun_ident ++ " prometeu proceder e não funcionar!"
    --   liftIO $ putStrLn $ emojis ++ "\n"
    
    state <- getState

    if isRunning state then do
      (fun_value, fun_typε) <- evalFun fun_ident $ fmap fst args
      pure (fun_value, fun_typε)

    else do
      enterBlock    <* unifyParams fun_params args
      default_value <- defaultValue return_type <* quitBlock
      pure (default_value, return_type)
          
    

-- | Parses dynamic array literals (@[1, 2, 3]@).
eDynArray = do
  openBracket_T   <- Token.openBracket
  (args, args_Ts) <- expr `sepBy'` Token.comma

  closeBracket_T  <- Token.closeBracket

  let (argsVs, argsTs) = unzip args
  unless (length (List.nub argsTs) <= 1) $ do
    -- Get first mismatched type for error message
    let firstType = case argsTs of (t:_) -> t; [] -> TAny
        differentTypes = List.nub argsTs
        mismatchedType = case filter (/= firstType) differentTypes of
          (t:_) -> t
          [] -> TAny
    
    let line = tokenLine openBracket_T
        col = tokenCol openBracket_T
        y = Text.unpack ErrFmt.yellow
        b = Text.unpack ErrFmt.blue
        c = Text.unpack ErrFmt.cyan
        bo = Text.unpack ErrFmt.bold
        r = Text.unpack ErrFmt.reset
        
        typeToStr t = case t of
          TInt -> "int"
          TFloat -> "float"
          TText -> "text"
          TBool -> "bool"
          TCustom name -> name
          TDynArray inner -> "[" ++ typeToStr inner ++ "]"
          TRef inner -> "&" ++ typeToStr inner
          _ -> show t
    
    liftIO $ TIO.putStrLn $ Text.pack $
      "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in array literal" ++ r ++ "\n" ++
      "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
      "   " ++ b ++ "|" ++ r ++ "\n" ++
      show line ++ " " ++ b ++ "|" ++ r ++ " [elem1, elem2, ...]\n" ++
      "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^^^^^^^^^^^^^^" ++ r ++ "\n" ++
      "   " ++ b ++ "|" ++ r ++ "\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "array elements must all have the same type\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "expected type: " ++ bo ++ typeToStr firstType ++ r ++ "\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "   found type: " ++ bo ++ typeToStr mismatchedType ++ r ++ "\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "ensure all elements are of type " ++ c ++ typeToStr firstType ++ r ++ "\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "or use explicit type conversions: " ++ 
      c ++ "float2int(), int2float()" ++ r ++ "\n"
    
    fail "* Array elements must all have the same type"

  let arrayT = case argsTs of [] -> TAny; (t:_) -> t
  let tokens = [openBracket_T] ++ args_Ts ++ [closeBracket_T]
  pure ((VDynArray $ Vector.fromList argsVs, TDynArray arrayT)
        , tokens)


-- | Reads input from stdin based on expected type.
--
-- Uses Haskeline for advanced line editing (backspace, arrow keys, history).
-- This provides a much better user experience than plain getLine.
readT :: Typε -> Interpreter Value
readT typε = do
  liftIO $ IO.hFlush IO.stdout  -- Flush stdout before reading input
  
  -- Use Haskeline for better input editing (backspace, arrows, history)
  maybeLine <- liftIO $ Haskeline.runInputT Haskeline.defaultSettings $ do
    Haskeline.getInputLine ""  -- Empty prompt (user code should print its own)
  
  case maybeLine of
    Nothing -> do
      -- Professional error: EOF while reading input
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ Text.unpack ErrFmt.yellow ++ "error" ++ Text.unpack ErrFmt.reset ++ 
        Text.unpack ErrFmt.bold ++ "[E0001]: end of input" ++ Text.unpack ErrFmt.reset ++ "\n" ++
        "  " ++ Text.unpack ErrFmt.blue ++ "--> " ++ Text.unpack ErrFmt.reset ++ "<stdin>\n" ++
        "   " ++ Text.unpack ErrFmt.blue ++ "|" ++ Text.unpack ErrFmt.reset ++ "\n" ++
        "   " ++ Text.unpack ErrFmt.blue ++ "= " ++ Text.unpack ErrFmt.reset ++ 
        Text.unpack ErrFmt.bold ++ "note: " ++ Text.unpack ErrFmt.reset ++ 
        "the input stream was closed while waiting for input\n" ++
        "   " ++ Text.unpack ErrFmt.blue ++ "= " ++ Text.unpack ErrFmt.reset ++ 
        Text.unpack ErrFmt.bold ++ "note: " ++ Text.unpack ErrFmt.reset ++ 
        "this typically happens when pressing Ctrl+D (Unix/Linux/macOS) or Ctrl+Z (Windows)\n" ++
        "   " ++ Text.unpack ErrFmt.blue ++ "= " ++ Text.unpack ErrFmt.reset ++ 
        Text.unpack ErrFmt.bold ++ "help: " ++ Text.unpack ErrFmt.reset ++ 
        "provide valid input or handle EOF in your program logic\n"
      fail "EOF: Input stream closed"
    Just lineStr -> do
      let line = Text.pack lineStr
      
      case typε of 
        TBool  -> do
          case Text.unpack line of
            "true"  -> pure $ VBool True
            "false" -> pure $ VBool False
            _ -> fail $ 
              "Invalid input for bool. "
              ++ "Expected \"true\" or \"false\", got: " 
              ++ Text.unpack line

        TInt   -> do
          case TR.signed TR.decimal line of
              Right (n, rest) | Text.null rest -> return (VInt n)
              _ -> fail $ 
                "Invalid input for int."
                ++ "Not a valid integer: " 
                ++ Text.unpack line

        TFloat -> do 
          case TR.rational line of
              Right (d, rest) | Text.null rest -> return (VFloat d)
              _ -> fail $ 
                "Invalid input for float." 
                ++ "Not a valid number: " 
                ++ Text.unpack line

        TText -> do
          return (VText line)

        _ -> fail $ 
          "read is a builtin which only" 
          ++ "works with \"bool\", \"int\","
          ++ "\"float\", \"text\""
  

-- | @scan(Type)@ - Reads input from stdin, converting to the specified type.
eScan :: Interpreter (ValueInfo, [Token])
eScan = do
  scan_T         <- Token.scan
  openParens_T   <- Token.openParens
  (typε, typε_Ts) <- Token.typε
  closeParans_T  <- Token.closeParens

  unless (typε `elem` [TBool, TInt, TFloat, TText]) .
      fail $ 
        "read is a builtin which only" 
        ++ "works with \"bool\", \"int\","
        ++ "\"float\", \"text\""

  let tokens = 
        [ scan_T
        , openParens_T]
        ++ typε_Ts ++
        [ closeParans_T
        ]
  value <- do  
    state <- getState
    if isRunning state 
      then readT typε
      else defaultValue typε

  pure ((value, typε), tokens) 


-- | @new(Type)@ - Allocates memory on the heap and returns a reference.
eNew :: Interpreter (ValueInfo, [Token])
eNew = do
  new_T <- Token.new
  openParens_T <- Token.openParens
  (allocType, type_Ts) <- Token.typε
  closeParens_T <- Token.closeParens
  
  let tokens = [new_T, openParens_T] ++ type_Ts ++ [closeParens_T]
  
  state <- getState
  if isRunning state then do
    -- Allocate on heap during execution
    let addr = view heapAddr state
    defaultVal <- defaultValue allocType
    
    -- Add to heap
    modifyState $ over heap (Map.insert addr (defaultVal, allocType))
    -- Increment heap address counter
    modifyState $ over heapAddr succ
    
    -- Return reference to heap
    let heapRef = VRef (HeapRef addr)
    pure ((heapRef, TRef allocType), tokens)
  else do
    -- During analysis, return null reference
    pure ((VNull, TRef allocType), tokens)


-- | @int2float(expr)@ - Converts an integer expression to float.
--
-- Type checks that the argument is TInt, then converts using Haskell's @fromInteger@.
--
-- @
-- int2float(42)      -- Returns: (VFloat 42.0, TFloat)
-- int2float(x + 10)  -- Converts result of integer expression
-- @
eInt2Float :: Interpreter (ValueInfo, [Token])
eInt2Float = do
  int2float_T <- Token.int2float
  openParens_T <- Token.openParens
  ((exprValue, exprType), expr_Ts) <- expr
  closeParens_T <- Token.closeParens

  -- Type checking
  unless (exprType == TInt) $
    typeError int2float_T TInt exprType 
      (Just "int2float() converts integers to floats. Use a variable or expression of type int")

  let tokens = [int2float_T, openParens_T] ++ expr_Ts ++ [closeParens_T]

  -- Conversion
  let convertedValue = case exprValue of
        VInt n -> VFloat (fromInteger n)
        _ -> VFloat 0.0  -- Fallback for Analysing phase

  pure ((convertedValue, TFloat), tokens)


-- | @float2int(expr)@ - Converts a float expression to integer (truncates).
--
-- Type checks that the argument is TFloat, then converts using Haskell's @truncate@.
--
-- @
-- float2int(3.14)      -- Returns: (VInt 3, TInt)
-- float2int(x / 2.0)   -- Converts result of float expression
-- @
eFloat2Int :: Interpreter (ValueInfo, [Token])
eFloat2Int = do
  float2int_T <- Token.float2int
  openParens_T <- Token.openParens
  ((exprValue, exprType), expr_Ts) <- expr
  closeParens_T <- Token.closeParens

  -- Type checking
  unless (exprType == TFloat) $
    typeError float2int_T TFloat exprType 
      (Just "float2int() converts floats to integers (truncates towards zero). Use a float expression")

  let tokens = [float2int_T, openParens_T] ++ expr_Ts ++ [closeParens_T]

  -- Conversion (truncate towards zero)
  let convertedValue = case exprValue of
        VFloat x -> VInt (truncate x)
        _ -> VInt 0  -- Fallback for Analysing phase

  pure ((convertedValue, TInt), tokens)


-- | @is_null(ptr)@ - Checks if a pointer is null.
--
-- Returns true if the pointer is null, false otherwise.
-- Works with any reference type: @&int@, @&Point@, etc.
--
-- @
-- is_null(p)           -- Returns: (VBool true, TBool) if p is null
-- is_null(&x)          -- Returns: (VBool false, TBool) - address-of is never null
-- if is_null(node.left) { ... }  -- Common usage in data structures
-- @
eIsNull :: Interpreter (ValueInfo, [Token])
eIsNull = do
  isNull_T <- Token.isNull
  openParens_T <- Token.openParens
  ((ptrValue, ptrType), ptr_Ts) <- expr
  closeParens_T <- Token.closeParens

  -- Type checking: must be a reference type
  case ptrType of
    TRef _ -> pure ()
    _ -> typeError isNull_T (TRef TUnit) ptrType
           (Just "is_null() checks if a pointer is null. Argument must be a reference type (&T)")

  let tokens = [isNull_T, openParens_T] ++ ptr_Ts ++ [closeParens_T]

  -- Check if pointer is null or invalid (freed)
  state <- getState
  resultValue <- if isRunning state then
    case ptrValue of
      VNull -> pure (VBool True)
      VRef ref -> do
        -- Check if reference is valid
        isValid <- safeResolveRef ref
        pure $ VBool (Maybe.isNothing isValid)
      _ -> pure (VBool False)
  else
    -- During analysis, just check for VNull
    pure $ case ptrValue of
      VNull -> VBool True
      _ -> VBool False

  pure ((resultValue, TBool), tokens)


--------------------------------------------------------- STMTS ---------------------------------------------------------

-- | Statement parsers (declarations, assignments, return, etc.).
sDeclVar, sDeclConst, sAtrib :: Interpreter [Token]

-- | Variable/const declarations with optional initialization (local scope).
sDeclVarInit, sDeclConstInit :: Interpreter [Token]
sDeclVarInit = do
  var_T <- Token.var
  (varIdent, varIdent_T) <- Token.varId
  colon_T <- Token.colon
  (varTypε, typε_Ts) <- Token.typε
  
  -- Optional initialization
  mInit <- optionMaybe $ do
    assign_T <- Token.assign
    ((value, exprTypε), expr_Ts) <- expr
    unless (exprTypε == varTypε) $
      typeError assign_T varTypε exprTypε
        (Just "Variable declaration requires matching types")
    pure (value, expr_Ts, assign_T)
  
  case mInit of
    Just (value, expr_Ts, assign_T) -> do
      -- Store the value in BOTH phases to track initialization
      void $ addVar varIdent varTypε (Just value) False
      semiColon_T <- Token.semiColon
      pure $ [var_T, varIdent_T, colon_T] ++ typε_Ts ++ [assign_T] ++ expr_Ts ++ [semiColon_T]
    Nothing -> do
      void $ addVar varIdent varTypε Nothing False
      semiColon_T <- Token.semiColon
      pure $ [var_T, varIdent_T, colon_T] ++ typε_Ts ++ [semiColon_T]

sDeclConstInit = do
  const_T <- Token.const
  (varIdent, varIdent_T) <- Token.varId
  colon_T <- Token.colon
  (varTypε, typε_Ts) <- Token.typε
  
  -- Optional initialization (but recommended for const!)
  mInit <- optionMaybe $ do
    assign_T <- Token.assign
    ((value, exprTypε), expr_Ts) <- expr
    unless (exprTypε == varTypε) $
      typeError assign_T varTypε exprTypε
        (Just "Const declaration requires matching types")
    pure (value, expr_Ts, assign_T)
  
  case mInit of
    Just (value, expr_Ts, assign_T) -> do
      -- Store the value in BOTH phases to track initialization
      void $ addVar varIdent varTypε (Just value) True
      semiColon_T <- Token.semiColon
      pure $ [const_T, varIdent_T, colon_T] ++ typε_Ts ++ [assign_T] ++ expr_Ts ++ [semiColon_T]
    Nothing -> do
      void $ addVar varIdent varTypε Nothing True
      semiColon_T <- Token.semiColon
      pure $ [const_T, varIdent_T, colon_T] ++ typε_Ts ++ [semiColon_T]

-- | Global variable declaration (adds to 'rtGlobals').
gDeclVar, gDeclConst :: Interpreter ()
gDeclVar = do
  _ <- Token.var
  (varIdent, _) <- Token.varId
  _ <- Token.colon
  (varTypε, _) <- Token.typε
  
  -- Optional initialization: var x: int = 42;
  mInit <- optionMaybe $ do
    _ <- Token.assign
    ((value, exprTypε), _) <- expr
    unless (exprTypε == varTypε) $
      fail $ "* Type mismatch in global initialization: expected " ++ show varTypε ++ " but got " ++ show exprTypε
    pure value
  
  state <- getState
  case mInit of
    Just value ->
      -- With initialization: add in Analysing, update in Running
      if isRunning state then
        updateGlobalVar varIdent value
      else
        addGlobalVar varIdent varTypε (Just value) False
    Nothing ->
      -- Without initialization: only add in Analysing
      unless (isRunning state) $
        addGlobalVar varIdent varTypε Nothing False
    
  _ <- Token.semiColon
  pure ()

gDeclConst = do
  _ <- Token.const
  (varIdent, _) <- Token.varId
  _ <- Token.colon
  (varTypε, _) <- Token.typε
  
  -- REQUIRED initialization for const: const x: int = 42;
  _ <- Token.assign
  ((value, exprTypε), _) <- expr
  unless (exprTypε == varTypε) $
    fail $ "* Type mismatch in const initialization: expected " ++ show varTypε ++ " but got " ++ show exprTypε
  
  state <- getState
  -- Add in Analysing, skip in Running (const doesn't change)
  unless (isRunning state) $
    addGlobalVar varIdent varTypε (Just value) True
    
  _ <- Token.semiColon
  pure ()

sDeclVar = do
  var_T <- Token.var

  (varIdent, varIdent_T) <- Token.varId
  colon_T <- Token.colon

  (varTypε, typε_Ts) <- Token.typε
  
  -- Optional initialization: var x: int = 42;
  mInit <- optionMaybe $ do
    assign_T <- Token.assign
    ((value, exprTypε), expr_Ts) <- expr
    unless (exprTypε == varTypε) $
      typeError assign_T varTypε exprTypε 
        (Just $ "Try changing the variable type or converting the value")
    pure ((value, assign_T : expr_Ts))
  
  case mInit of
    Just (value, init_Ts) -> do
      void $ addVar varIdent varTypε (Just value) False
      semiColon_T <- Token.semiColon
      pure $ [var_T, varIdent_T, colon_T] ++ typε_Ts ++ init_Ts ++ [semiColon_T]
    Nothing -> do
      void $ addVar varIdent varTypε Nothing False
      semiColon_T <- Token.semiColon
      pure $ [var_T, varIdent_T, colon_T] ++ typε_Ts ++ [semiColon_T]

-- | Local const declaration (requires initialization).
sDeclConst = do
  const_T <- Token.const

  (varIdent, varIdent_T) <- Token.varId
  colon_T <- Token.colon

  (varTypε, typε_Ts) <- Token.typε
  
  -- REQUIRED initialization for const: const pi: float = 3.14;
  assign_T <- Token.assign
  ((value, exprTypε), expr_Ts) <- expr
  unless (exprTypε == varTypε) $
    fail $ "Type mismatch in const initialization: expected " 
           ++ show varTypε ++ ", got " ++ show exprTypε
  
  void $ addVar varIdent varTypε (Just value) True
  semiColon_T <- Token.semiColon
  pure $ [const_T, varIdent_T, colon_T] ++ typε_Ts ++ [assign_T] ++ expr_Ts ++ [semiColon_T]

-- | Variable assignment statement (@x = value;@).
--
-- Uses 'searchVarInfoForAssignment' to avoid false "uninitialized" errors
-- when assigning to a variable for the first time.
sAtrib = do
  (varIdent, varIdent_T) <- Token.varId

  -- Use special version that doesn't check initialization (we're about to initialize it!)
  (prefix , VarInfo _mValue varTypε constFlag _) <- searchVarInfoForAssignment varIdent

  assign_T <- Token.assign
  ((value , exprTypε), expr_Ts) <- expr

  unless (exprTypε == varTypε) $
    typeError assign_T varTypε exprTypε 
      (Just $ "Cannot assign " ++ show exprTypε ++ " to variable of type " ++ show varTypε)

  when constFlag $
    compileError assign_T "Cannot assign to const variable" $
      "Variable '" ++ varIdent ++ "' is declared as const and cannot be modified.\n" ++
      "help: Consider declaring it as 'var' instead of 'const' if you need to modify it"

  semiColon_T <- Token.semiColon

  -- Update variable value in BOTH phases to track initialization
  if prefix == -1
    then updateGlobalVar varIdent value  -- Global variable
    else updateVarAt prefix varIdent value  -- Local variable

  pure $ [varIdent_T, assign_T] ++ expr_Ts ++ [semiColon_T]


-- | Print statement (@print(expr);@).
--
-- Prints a value followed by a newline. Line endings are automatically
-- normalized for the platform (Windows uses CRLF, Unix/Linux/macOS use LF).
sPrint :: Interpreter [Token]
sPrint = do
  print_T       <- Token.print

  openParens_T  <- Token.openParens
  ((value, _valueType), value_Ts) <- expr
  closeParens_T <- Token.closeParens

  semiColon_T   <- Token.semiColon

  state <- getState
  when (isRunning state) $ do
    -- Only dereference pointers inside structs
    printValue <- case value of
      VStruct {} -> dereferenceStructPointers value
      _ -> pure value
    
    liftIO $ do
      let output = show printValue
      -- Fix portability: normalize line endings in the output
      putStrLn (normalizeLineEndings output)
      IO.hFlush IO.stdout

  pure $
    [print_T,openParens_T]
    ++ value_Ts ++
    [closeParens_T, semiColon_T]

-- | Dereference pointers in struct fields for pretty printing
-- Only called when printing structs, leaves other values unchanged
-- Safely handles invalid/freed pointers by showing "<dead>" instead of crashing
dereferenceStructPointers :: Value -> Interpreter Value
dereferenceStructPointers (VStruct ident fields) = do
  -- Dereference each field
  derefFields <- Map.traverseWithKey dereferenceField fields
  pure $ VStruct ident derefFields
  where
    dereferenceField :: Ident -> (Maybe Value, Typε) -> Interpreter (Maybe Value, Typε)
    dereferenceField _ (Nothing, typ) = pure (Nothing, typ)
    dereferenceField _ (Just (VRef ref), TRef innerType) = do
      -- Safely try to dereference the pointer
      safeDeref <- safeResolveRef ref
      case safeDeref of
        Just (derefVal, _) -> do
          -- Recursively handle nested structs
          finalVal <- case derefVal of
            VStruct {} -> dereferenceStructPointers derefVal
            _ -> pure derefVal
          pure (Just finalVal, TRef innerType)
        Nothing ->
          -- Pointer is invalid (freed/dangling) - show as special marker
          pure (Just (VText (Text.pack "<dead>")), TRef innerType)
    dereferenceField _ (Just val, typ) = do
      -- Recursively handle nested structs
      derefVal <- case val of
        VStruct {} -> dereferenceStructPointers val
        _ -> pure val
      pure (Just derefVal, typ)
dereferenceStructPointers other = pure other

-- | Safely resolve a reference without crashing if invalid
-- Returns Nothing if the reference is to freed/invalid memory
safeResolveRef :: Ref -> Interpreter (Maybe ValueInfo)
safeResolveRef ref = case ref of
  StackRef ident prefix -> do
    -- Stack references should always be valid during execution
    result <- searchVarValueAt prefix ident
    pure (Just result)
  
  HeapRef addr -> do
    -- Check if heap address is still valid
    currentHeap <- view heap <$> getState
    pure $ Map.lookup addr currentHeap
  
  FieldRef baseRef fieldIdent -> do
    baseResult <- safeResolveRef baseRef
    case baseResult of
      Nothing -> pure Nothing
      Just (VStruct _ fields, _) -> 
        case Map.lookup fieldIdent fields of
          Just (Just val, typ) -> pure (Just (val, typ))
          _ -> pure Nothing
      Just _ -> pure Nothing
  
  IndexRef baseRef idx -> do
    baseResult <- safeResolveRef baseRef
    case baseResult of
      Nothing -> pure Nothing
      Just (VDynArray arr, TDynArray elemType) ->
        if idx >= 0 && idx < toInteger (Vector.length arr)
        then pure (Just (arr Vector.! fromInteger idx, elemType))
        else pure Nothing
      Just _ -> pure Nothing


-- | Printf statement (@printf(format_string, args...);@) - C-style formatted output.
--
-- Accepts a format string followed by variable arguments for interpolation.
-- Uses Haskell's Text.Printf for formatting.
--
-- Supported format specifiers:
-- * @%d@ - Integer
-- * @%f@ - Float
-- * @%s@ - Text/String
-- * @%%@ - Literal percent sign
--
-- @
-- printf("Hello, World!\n");
-- printf("Number: %d\n", 42);
-- printf("Pi: %f\n", 3.14159);
-- printf("Text: %s\n", "hello");
-- printf("Multiple: %d %f %s\n", 10, 3.14, "test");
-- @
sPrintf :: Interpreter [Token]
sPrintf = do
  printf_T      <- Token.printf
  openParens_T  <- Token.openParens
  
  -- Parse format string (must be text)
  ((formatValue, formatType), format_Ts) <- expr
  unless (formatType == TText) $
    typeError printf_T TText formatType
      (Just "printf() requires a text format string as the first argument")

  -- Parse optional arguments separated by comma
  (args, args_Ts) <- option ([], []) $ do
    comma_T <- Token.comma
    (argList, argTokens) <- expr `sepBy'` Token.comma
    pure (argList, comma_T : argTokens)

  closeParens_T <- Token.closeParens
  semiColon_T   <- Token.semiColon

  -- Runtime execution
  state <- getState
  when (isRunning state) $ do
    case formatValue of
      VText formatStr -> do
        -- Convert arguments to printable strings
        let formatArgs = fmap (valueToFormatArg . fst) args
        -- Use Text.Printf-style formatting
        let output = formatPrintf (Text.unpack formatStr) formatArgs
        -- Fix portability issues:
        -- #1: Normalize line endings for the platform (\n -> \r\n on Windows)
        let normalizedOutput = normalizeLineEndings output
        -- #3: Always flush stdout after printf (ensures immediate output)
        liftIO $ do
          putStr normalizedOutput
          IO.hFlush IO.stdout
      _ -> do
        let line = tokenLine printf_T
            col = tokenCol printf_T
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " printf(format, ...);\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "       ^^^^^^" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "printf() format must be of type " ++ bo ++ "text" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "use a string literal: " ++ 
          c ++ "printf(\"Hello %d\\n\", 42);" ++ r ++ "\n"
        
        fail "Runtime Error: printf format must be text value"

  pure $
    [printf_T, openParens_T]
    ++ format_Ts
    ++ args_Ts
    ++ [closeParens_T, semiColon_T]

-- | Convert a Value to a format argument for printf
valueToFormatArg :: Value -> String
valueToFormatArg (VInt n) = show n
valueToFormatArg (VFloat x) = show x
valueToFormatArg (VText t) = Text.unpack t
valueToFormatArg (VBool b) = if b then "true" else "false"
valueToFormatArg VUnit = "()"
valueToFormatArg VNull = "null"
valueToFormatArg v = show v

-- | Normalize line endings based on operating system.
--
-- Fix portability issue #1: Converts @\\n@ to platform-specific line endings.
--
-- * __Unix/Linux/macOS__: @\\n@ (LF)
-- * __Windows__: @\\r\\n@ (CRLF)
--
-- This ensures consistent behavior across all platforms when printing text.
normalizeLineEndings :: String -> String
normalizeLineEndings s
  | SysInfo.os == "mingw32" = concatMap (\c -> if c == '\n' then "\r\n" else [c]) s
  | otherwise = s

-- | Simple printf-style formatter (supports %d, %f, %s, %%)
formatPrintf :: String -> [String] -> String
formatPrintf [] _ = []
formatPrintf ('%':'%':rest) args = '%' : formatPrintf rest args
formatPrintf ('%':'d':rest) (arg:args) = arg ++ formatPrintf rest args
formatPrintf ('%':'f':rest) (arg:args) = arg ++ formatPrintf rest args
formatPrintf ('%':'s':rest) (arg:args) = arg ++ formatPrintf rest args
formatPrintf ('%':c:rest) args = 
  -- Unknown format specifier, just pass through
  '%' : c : formatPrintf rest args
formatPrintf (c:rest) args = c : formatPrintf rest args


-- | Abort statement (@abort(message);@) - Panic/error abort with message.
--
-- Immediately terminates program execution and prints error message to stderr.
-- Similar to Rust's @panic!()@ macro.
--
-- @
-- if (denominator == 0) {
--     abort("Division by zero!");
-- }
-- @
sAbort :: Interpreter [Token]
sAbort = do
  abort_T       <- Token.abort
  openParens_T  <- Token.openParens
  
  -- Parse error message (must be text)
  ((msgValue, msgType), msg_Ts) <- expr
  unless (msgType == TText) $
    typeError abort_T TText msgType
      (Just "abort() requires a text message to display when panicking")
  
  closeParens_T <- Token.closeParens
  semiColon_T   <- Token.semiColon

  -- Runtime execution - always abort!
  state <- getState
  when (isRunning state) $ do
    case msgValue of
      VText msg -> liftIO $ do
        -- Print error to stderr with professional formatting
        let y = Text.unpack ErrFmt.yellow
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        IO.hPutStrLn IO.stderr $ "\n" ++ y ++ "runtime error" ++ r ++ bo ++ ": program aborted" ++ r
        IO.hPutStrLn IO.stderr $ bo ++ "message: " ++ r ++ Text.unpack msg
        IO.hFlush IO.stderr
        -- Exit with error code
        Exit.exitFailure
      _ -> do
        let line = tokenLine abort_T
            col = tokenCol abort_T
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " abort(message);\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "      ^^^^^^^" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "abort() message must be of type " ++ bo ++ "text" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "use a string literal: " ++ 
          c ++ "abort(\"Something went wrong!\");" ++ r ++ "\n"
        
        fail "Runtime Error: abort message must be text value"

  pure $
    [abort_T, openParens_T]
    ++ msg_Ts
    ++ [closeParens_T, semiColon_T]


-- | @drop(ref)@ - Deallocates heap memory.
-- | Drop statement (@drop(ref);@) - Deallocates heap memory.
--
-- Removes a heap-allocated value from memory. The reference becomes invalid after drop.
-- Attempting to use a dropped reference results in runtime error.
--
-- @
-- var x: &int = new int(42);
-- drop(x);  -- x is now invalid
-- @
sDrop :: Interpreter [Token]
sDrop = do
  drop_T <- Token.drop
  openParens_T <- Token.openParens
  ((refValue, refType), ref_Ts) <- expr
  closeParens_T <- Token.closeParens
  semiColon_T <- Token.semiColon
  
  let tokens = [drop_T, openParens_T] ++ ref_Ts ++ [closeParens_T, semiColon_T]
  
  -- Compile-time check: must be a reference type
  case refType of
    TRef _ -> pure ()
    _ -> typeError drop_T (TRef TUnit) refType
           (Just $ "drop() is used to deallocate heap memory. It requires a reference (created with 'new')")
  
  -- Runtime execution
  state <- getState
  when (isRunning state) $ do
    case refValue of
      VNull -> runtimeError drop_T "Cannot drop null reference" 
                 (Just "Make sure the reference is valid before calling drop()")
      VRef (HeapRef addr) -> do
        -- Check if address exists in heap
        currentHeap <- view heap <$> getState
        case Map.lookup addr currentHeap of
          Nothing -> runtimeError drop_T "Use after free or double free"
                       (Just $ "Heap address " ++ show addr ++ " was already freed or never allocated")
          Just _ -> do
            -- Remove from heap
            modifyState $ over heap (Map.delete addr)
      VRef (StackRef ident _) -> 
        runtimeError drop_T ("Cannot drop stack reference '&" ++ ident ++ "'")
          (Just "Only heap-allocated references (created with 'new') can be dropped")
      VRef (IndexRef _ _) ->
        runtimeError drop_T "Cannot drop indexed reference"
          (Just "Drop the base reference instead of an array element")
      VRef (FieldRef _ _) ->
        runtimeError drop_T "Cannot drop field reference"
          (Just "Drop the struct reference instead of a single field")
      _ -> do
        let y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
            line = tokenLine drop_T
            col = tokenCol drop_T
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E9999]: internal compiler error" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "expected reference value in drop() but got " ++ c ++ show refType ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "drop() should only accept reference values (type &T)\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "this is a compiler bug, please report it with:\n" ++
          "   " ++ b ++ "=" ++ r ++ "        - the source code that triggered this error\n" ++
          "   " ++ b ++ "=" ++ r ++ "        - value type: " ++ c ++ show refType ++ r ++ "\n"
        fail "Internal Error: Expected reference value in drop()"
  
  pure tokens


-- | Array cons statement (@elem >>= array;@) - Prepends element to beginning (imperativo).
--
-- Modifies the array in-place by adding the element at index 0.
-- Supports l-values: simple variables, struct fields, array indexing, and references.
--
-- @
-- var xs: [int];
-- xs = [2, 3, 4];
-- 1 >>= xs;  -- xs becomes [1, 2, 3, 4]
-- 
-- struct Person { friends: [text]; }
-- var alice: Person;
-- "Eve" >>= alice.friends;  -- Now works!
-- @
sArrayCons :: Interpreter [Token]
sArrayCons = do
  ((elemValue, elemType), elem_Ts) <- expr
  cons_T <- Token.arrayCons
  (arrIdent, arrIdent_T) <- Token.varId
  
  -- Get base reference and use lValueChain to parse field access, indexing, etc.
  (prefix, VarInfo _mValue baseType _const _) <- searchVarInfo arrIdent
  let baseRef = StackRef arrIdent prefix
  ((finalRef, arrType), chainTokens) <- lValueChain (baseRef, baseType)
  
  semiColon_T <- Token.semiColon
  
  -- Type checking: verify variable exists and is an array
  case arrType of
    TDynArray innerType -> do
      unless (elemType == innerType) $ do
        -- Professional error: type mismatch in array cons
        let line = tokenLine cons_T
            col = tokenCol cons_T
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in array operation" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " elem >: array\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "^^^^" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "= " ++ r ++ bo ++ "note: " ++ r ++ "cannot prepend " ++ c ++ show elemType ++ r ++ " to array of " ++ c ++ show innerType ++ r ++ "\n" ++
          "   " ++ b ++ "= " ++ r ++ bo ++ "help: " ++ r ++ "ensure element type matches array element type\n" ++
          "   " ++ b ++ "= " ++ r ++ bo ++ "help: " ++ r ++ "or use type conversion: " ++ c ++ "int2float(), float2int()" ++ r ++ "\n"
        
        fail $ "Type Error: Cannot prepend " ++ show elemType ++ " to array of " ++ show innerType
    _ -> do
      -- Professional error: operator expects array
      let line = tokenLine arrIdent_T
          col = tokenCol arrIdent_T
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " elem >>= " ++ arrIdent ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ concat (replicate (13 + length arrIdent) " ") ++ y ++ concat (replicate (length arrIdent) "^") ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "= " ++ r ++ bo ++ "note: " ++ r ++ "operator " ++ c ++ ">>=" ++ r ++ " expects an array on the right\n" ++
        "   " ++ b ++ "= " ++ r ++ bo ++ "note: " ++ r ++ "but got: " ++ c ++ show arrType ++ r ++ "\n" ++
        "   " ++ b ++ "= " ++ r ++ bo ++ "help: " ++ r ++ "use an array variable or array literal: " ++ c ++ "[1, 2, 3]" ++ r ++ "\n"
      
      fail $ "Type Error: '>>=' expects an array on the right, but got " ++ show arrType
  
  -- Runtime: prepend element to array
  state <- getState
  when (isRunning state) $ do
    (VDynArray arr, _) <- resolveRef finalRef
    let newArr = VDynArray (Vector.cons elemValue arr)
    writeToRef finalRef newArr
  
  pure $ elem_Ts ++ [cons_T, arrIdent_T] ++ chainTokens ++ [semiColon_T]


-- | Array push statement (@array =<< elem;@) - Appends element to end (imperativo).
--
-- Modifies the array in-place by adding the element at the end.
-- Supports l-values: simple variables, struct fields, array indexing, and references.
--
-- @
-- var xs: [int];
-- xs = [1, 2, 3];
-- xs =<< 4;  -- xs becomes [1, 2, 3, 4]
-- 
-- struct Person { friends: [text]; }
-- var alice: Person;
-- alice.friends =<< "Diana";  -- Now works!
-- @
sArrayPush :: Interpreter [Token]
sArrayPush = do
  (arrIdent, arrIdent_T) <- Token.varId
  
  -- Get base reference and use lValueChain to parse field access, indexing, etc.
  (prefix, VarInfo _mValue baseType _const _) <- searchVarInfo arrIdent
  let baseRef = StackRef arrIdent prefix
  ((finalRef, arrType), chainTokens) <- lValueChain (baseRef, baseType)
  
  push_T <- Token.arrayPush
  ((elemValue, elemType), elem_Ts) <- expr
  semiColon_T <- Token.semiColon
  
  -- Type checking
  case arrType of
    TDynArray innerType -> do
      unless (elemType == innerType) $ do
        -- Professional error: type mismatch in array push
        let line = tokenLine push_T
            col = tokenCol push_T
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch in array operation" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " array =<< elem\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "       ^^^" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "= " ++ r ++ bo ++ "note: " ++ r ++ "cannot append " ++ c ++ show elemType ++ r ++ " to array of " ++ c ++ show innerType ++ r ++ "\n" ++
          "   " ++ b ++ "= " ++ r ++ bo ++ "help: " ++ r ++ "ensure element type matches array element type\n" ++
          "   " ++ b ++ "= " ++ r ++ bo ++ "help: " ++ r ++ "or use type conversion: " ++ c ++ "int2float(), float2int()" ++ r ++ "\n"
        
        fail $ "Type Error: Cannot append " ++ show elemType ++ " to array of " ++ show innerType
    _ -> do
      -- Professional error: operator expects array
      let line = tokenLine arrIdent_T
          col = tokenCol arrIdent_T
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " " ++ arrIdent ++ " =<< elem\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ concat (replicate (length arrIdent) "^") ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "= " ++ r ++ bo ++ "note: " ++ r ++ "operator " ++ c ++ "=<<" ++ r ++ " expects an array on the left\n" ++
        "   " ++ b ++ "= " ++ r ++ bo ++ "note: " ++ r ++ "but got: " ++ c ++ show arrType ++ r ++ "\n" ++
        "   " ++ b ++ "= " ++ r ++ bo ++ "help: " ++ r ++ "declare as array: " ++ c ++ "var " ++ arrIdent ++ ": [type]" ++ r ++ "\n"
      
      fail $ "Type Error: '=<<' expects an array on the left, but got " ++ show arrType
  
  -- Runtime: append element to array
  state <- getState
  when (isRunning state) $ do
    (VDynArray arr, _) <- resolveRef finalRef
    let newArr = VDynArray (Vector.snoc arr elemValue)
    writeToRef finalRef newArr
  
  pure $ [arrIdent_T] ++ chainTokens ++ [push_T] ++ elem_Ts ++ [semiColon_T]


-- | Array drop statement (@array !<<;@) - Removes first element (imperativo).
--
-- Modifies the array in-place by removing the first element.
-- Does nothing if array is empty.
-- Supports l-values: simple variables, struct fields, array indexing, and references.
--
-- @
-- var xs: [int];
-- xs = [1, 2, 3];
-- xs !<<;  -- xs becomes [2, 3]
-- 
-- struct Person { friends: [text]; }
-- var alice: Person;
-- alice.friends !<<;  -- Now works!
-- @
sArrayDrop :: Interpreter [Token]
sArrayDrop = do
  (arrIdent, arrIdent_T) <- Token.varId
  
  -- Get base reference and use lValueChain to parse field access, indexing, etc.
  (prefix, VarInfo _mValue baseType _const _) <- searchVarInfo arrIdent
  let baseRef = StackRef arrIdent prefix
  ((finalRef, arrType), chainTokens) <- lValueChain (baseRef, baseType)
  
  drop_T <- Token.arrayDrop
  semiColon_T <- Token.semiColon
  
  -- Type checking: verify it's an array
  case arrType of
    TDynArray _innerType -> pure ()
    _ -> fail $ "Type Error: '!<<' expects an array, but got " ++ show arrType
  
  -- Runtime: drop first element (if not empty)
  state <- getState
  when (isRunning state) $ do
    (VDynArray arr, _) <- resolveRef finalRef
    unless (Vector.null arr) $ do
      let newArr = VDynArray (Vector.tail arr)
      writeToRef finalRef newArr
  
  pure $ [arrIdent_T] ++ chainTokens ++ [drop_T, semiColon_T]


-- | Array uncons statement (@array !>>;@) - Removes first element (imperativo).
--
-- Same as '!<<' (drop). Modifies the array in-place by removing the first element.
-- Does nothing if array is empty.
-- Supports l-values: simple variables, struct fields, array indexing, and references.
--
-- @
-- var xs: [int];
-- xs = [1, 2, 3];
-- xs !>>;  -- xs becomes [2, 3]
-- 
-- struct Person { tasks: [text]; }
-- var bob: Person;
-- bob.tasks !>>;  -- Now works!
-- @
sArrayUncons :: Interpreter [Token]
sArrayUncons = do
  (arrIdent, arrIdent_T) <- Token.varId
  
  -- Get base reference and use lValueChain to parse field access, indexing, etc.
  (prefix, VarInfo _mValue baseType _const _) <- searchVarInfo arrIdent
  let baseRef = StackRef arrIdent prefix
  ((finalRef, arrType), chainTokens) <- lValueChain (baseRef, baseType)
  
  uncons_T <- Token.arrayUncons
  semiColon_T <- Token.semiColon
  
  -- Type checking: verify it's an array
  case arrType of
    TDynArray _innerType -> pure ()
    _ -> fail $ "Type Error: '!>>' expects an array, but got " ++ show arrType
  
  -- Runtime: remove first element (if not empty)
  state <- getState
  when (isRunning state) $ do
    (VDynArray arr, _) <- resolveRef finalRef
    unless (Vector.null arr) $ do
      let newArr = VDynArray (Vector.tail arr)
      writeToRef finalRef newArr
  
  pure $ [arrIdent_T] ++ chainTokens ++ [uncons_T, semiColon_T]

-- | If-then-else statement with optional else branch.
sIfThenElse :: Interpreter [Token]
sIfThenElse = do
  if_T <- Token.iff

  ((cond, _), value_Ts) <- expr
  case cond of
    VBool b -> do
      let (evalIf, evalElse) = if b
          then (evalBlock, evalBlockInOff)
          else (evalBlockInOff, evalBlock)

      -- if
      ifArm_Ts   <- enterBlock *> evalIf <* quitBlock

      -- else
      elseArm_Ts <- option [] $ do
        else_T   <- Token.elsε
        block_Ts <- enterBlock *> evalElse <* quitBlock
        pure (else_T:block_Ts)

      pure $
        if_T:value_Ts
        ++ ifArm_Ts
        ++ elseArm_Ts


    _ -> do
      let line = tokenLine if_T
          col = tokenCol if_T
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " if (condition) { ... }\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "    ^^^^^^^^^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "condition in 'if' statement must be of type " ++ bo ++ "bool" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "use a comparison operator: " ++ c ++ "x == 0, x > 5, x != null" ++ r ++ "\n"
      
      fail "* Condition in 'if' statement must be a boolean expression"


-- | Helper for do-while pattern (executes body at least once).
doWhile :: Monad m => m () -> m Bool -> m ()
doWhile spell p = do
  spell
  b <- p
  when b $
    doWhile spell p


-- | While loop statement.
sWhile :: Interpreter [Token]
sWhile = do
  while_T <- Token.while
  _ <- getState
  ((stop_cond, _), expr_Ts) <- expr

  case stop_cond of
    VBool b -> do
      body <- loadBlock
      when b $ do
        -- Each iteration should have its own scope!
        doWhile
          (enterBlock >> evalBodyInTs body >> quitBlock)
          (handle $ exprInTs expr_Ts)

      pure $
        [while_T]
        ++ expr_Ts
        ++ body

    _ -> do
      let line = tokenLine while_T
          col = tokenCol while_T
          y = Text.unpack ErrFmt.yellow
          b = Text.unpack ErrFmt.blue
          c = Text.unpack ErrFmt.cyan
          bo = Text.unpack ErrFmt.bold
          r = Text.unpack ErrFmt.reset
      
      liftIO $ TIO.putStrLn $ Text.pack $
        "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: type mismatch" ++ r ++ "\n" ++
        "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        show line ++ " " ++ b ++ "|" ++ r ++ " while (condition) { ... }\n" ++
        "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "       ^^^^^^^^^" ++ r ++ "\n" ++
        "   " ++ b ++ "|" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "condition in 'while' loop must be of type " ++ bo ++ "bool" ++ r ++ "\n" ++
        "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "use a comparison operator: " ++ c ++ "x < 10, x != 0, done == false" ++ r ++ "\n"
      
      fail "* Condition in 'while' loop must be a boolean expression"
  where
    handle spell = do
      (stop_cond, _) <- spell
      case stop_cond of
        VBool b -> do
            s <- getState
            pure $ b && isRunning s
        _       -> do
          -- Professional internal error: unexpected non-boolean in while condition
          let y = Text.unpack ErrFmt.yellow
              r = Text.unpack ErrFmt.reset
              bo = Text.unpack ErrFmt.bold
          liftIO $ TIO.putStrLn $ Text.pack $
            "\n" ++ y ++ "error" ++ r ++ bo ++ "[E9999]: internal compiler error" ++ r ++ "\n" ++
            "   |\n" ++
            "   = " ++ bo ++ "note: " ++ r ++ "while condition evaluated to non-boolean value: " ++ show stop_cond ++ "\n" ++
            "   = " ++ bo ++ "note: " ++ r ++ "this should have been caught during type checking\n" ++
            "   = " ++ bo ++ "help: " ++ r ++ "please report this bug to the Gloom developers\n"
          fail "Internal Error: Unexpected non-boolean value in while condition"


-- | For-each loop statement (@for x in array { ... }@).
sForEach :: Interpreter [Token]
sForEach = do -- for i in is {..}
  for_T <- Token.for

  (index, index_T) <- Token.varId
  in_T <- Token.inn
  ((VDynArray array, TDynArray elems_type), array_Ts) <- expr

  let ass = [for_T, index_T, in_T] ++ array_Ts

  -- load/charging phase
  _ <- enterBlock *> addVar index elems_type Nothing False
  body <- loadBlock <* quitBlock
  forM_ array $ \x -> do
    enterBlock
    state <- getState
    void $ addVar index elems_type
      (if isRunning state then Just x else Nothing) False
    evalBodyInTs body
    quitBlock

  pure $ ass ++ body


-- | Wraps a parser to stop execution if a return statement was encountered.
handleReturns :: Interpreter [Token] -> Interpreter [Token]
handleReturns parser = do
  RTState {..} <- getState
  case rtFlag of
    Returning (Just _) -> pure []
    _ -> parser

-- ============================================================================
-- END OF STATEMENT PARSERS
-- ============================================================================

-- | Main statement parser (delegates to specific statement types).
stmt :: Interpreter [Token]
stmt = do
  choice 
    [ sReturn
    , sDeclVar
    , sDeclConst
    , sDrop           -- ← drop heap references
    , sPrintf         -- ← formatted print (must come before sPrint)
    , sAbort          -- ← panic/abort with error message
    , sPrint          -- ← keywords devem vir antes
    , sIfThenElse
    , sForEach
    , sWhile
    , try sDerefAtrib     -- ← *p = value (precisa vir antes de ident)
    , try sIdentBasedStmt     -- ← x = value ou x.field = value ou chamada de função (needs try to allow array ops to attempt!)
    -- Array operations come AFTER sIdentBasedStmt to prevent backtracking from duplicating function calls
    , try sArrayDrop  -- ← array !<<; (drop first element)
    , try sArrayUncons -- ← array !>>; (uncons first element)
    , try sArrayPush  -- ← array =<< elem; (append)
    , try sArrayCons  -- ← elem >>= array; (prepend) - needs try to avoid conflict
    ]


-- | Parses function call statement (procedure call that returns Unit).
-- Validates that the called function has Unit return type (is a procedure).
-- Example: @sFunCall@ parsing @"print(x);"@ validates print returns Unit, then evaluates arguments and calls function.
sFunCall :: Interpreter [Token]
sFunCall = do
  (fun_ident, fun_ident_T) <- Token.varId

  openParens_T    <- Token.openParens
  (args, args_Ts) <- (`sepBy'` Token.comma) expr
  closeParens_T   <- Token.closeParens

  semiColon_T     <- Token.semiColon

  state <- getState
  if isRunning state then do
    (_, fun_typε) <- evalFun fun_ident $ fmap fst args
    let has_return = fun_typε /= TUnit 
    when has_return . fail $ 
        fun_ident  ++ " has return of type: "
        ++ show fun_typε ++ ", but a procedure "
        ++ " expects: void"
  else do
    -- During analysis, just validate the function exists and check types
    mFun <- searchFun fun_ident
    case mFun of
      Nothing -> fail $ "Function not found: " ++ fun_ident
      Just (return_type, fun_params, _) -> do
        let has_return = return_type /= TUnit
        when has_return . fail $
            fun_ident  ++ " has return of type: "
            ++ show return_type ++ ", but a procedure "
            ++ " expects: void"
        -- Type check arguments
        when (length fun_params /= length args) $
          fail $ "Wrong number of arguments for " ++ fun_ident
        forM_ (zip fun_params args) $ \((_, paramType, _), (_, argType)) ->
          unless (argType == paramType) $
            fail $ "Type mismatch in call to " ++ fun_ident

  pure $
    [fun_ident_T, openParens_T] 
    ++ args_Ts ++ [closeParens_T, semiColon_T]

-- | Helper alias for evaluating block with tokens.
evalBLockInTs :: [Token] -> Interpreter ()
evalBLockInTs = evalBodyInTs

-- | Evaluates a block body from a token stream (used for conditional execution).
evalBodyInTs :: [Token] -> Interpreter ()
evalBodyInTs body_tokens = do
    main_stream <- getInput

    setInput body_tokens

    let block_parser =
          between Token.openBrace
                  Token.closeBrace
                  $ many stmt

    void block_parser

    setInput main_stream


-- | Evaluates an expression from a token stream.
exprInTs :: [Token] -> Interpreter ValueInfo
exprInTs expr_tokens = do
  main_stream <- getInput

  setInput expr_tokens

  (output, _) <- expr

  setInput main_stream
  pure output

-- | Evaluates a block during 'Running' phase.
evalBlock :: Interpreter [Token]
evalBlock = do
  openBrace_T  <- Token.openBrace
  stmts_Ts     <- many stmt
  closeBrace_T <- Token.closeBrace

  pure $
    openBrace_T:concat stmts_Ts
    ++ [closeBrace_T]


-- | Helper alias for loading block in analysis mode.
loadBlock :: Interpreter [Token]
loadBlock = evalBlockInOff

-- | Loads block structure without executing (for 'Analysing' phase).
evalBlockInOff :: Interpreter [Token]
evalBlockInOff = do
  RTState {..} <- getState
  turnOff
  
  openBrace_T  <- Token.openBrace
  stmts_Ts     <- many stmt
  closeBrace_T <- Token.closeBrace

  updateState $
    flag .~ rtFlag

  pure $
    openBrace_T:concat stmts_Ts
    ++ [closeBrace_T]

-- | Parses return statement with type checking.
-- Validates that returned expression type matches function's declared return type.
-- Sets interpreter flag to 'Returning' during Running phase.
-- Example: @sReturn@ parsing @"return 42;"@ validates Int type and stores value for function to return.
sReturn :: Interpreter [Token]
sReturn = do
  return_T              <- Token.return
  ((value, expr_type), expr_Ts) <- expr
  semiColon_T           <- Token.semiColon

  let tokens = return_T:expr_Ts ++ [semiColon_T]

  state@RTState {..} <- getState
  when (isRunning state) .
    updateState $
      -- (register ?~ value) . 
      (flag .~ Returning (Just (value, expr_type)))
  -- turnOff -- TODO: só no when????
  
  -- TODO: possible cause of future errors of return type checking
  when (rtReturn /= Just expr_type) $
    fail $ "* Return type mismatch: function expects to return " ++ show (Maybe.fromMaybe TUnit rtReturn) ++ " but got " ++ show expr_type

  pure tokens

-- | Function definition parser.
pFun :: Interpreter (Value, Typε)
pFun = do
  _ <- Token.fn
  (funIdent, funIdent_T) <- Token.varId

  -- BUG FIX: Don't change to Running here! This is done at start of phase 2
  -- when (funIdent == "main") . updateState $ flag .~ Running
  _ <- Token.openParens
  (params, _) <- (`sepBy'` Token.comma) $ do
    mConst_T <- optional Token.const
    (varIdent, varIdent_T) <- Token.varId
    colon_T                <- Token.colon
    (varType, varType_Ts)   <- Token.typε

    pure ((varIdent, varType, Maybe.isJust mConst_T),
      [varIdent_T, colon_T] ++ varType_Ts)
  _ <- Token.closeParens

  -- DONT FORGET THIS COLON TOKEN!!!
  mReturnType <- optional $ do
    colon_T <- Token.colon
    (typε, type_Ts) <- Token.typε
    pure (typε, colon_T:type_Ts)

  let (returnType, _) = Maybe.fromMaybe (TUnit, []) mReturnType

  RTState {..} <- getState
  updateState $ returning ?~ returnType

  args <- (\(_, t, _) -> defaultValue t) `mapM` params

  state <- getState

  addFunWithToken funIdent funIdent_T (returnType, params, Nothing)
  
  -- NOTE: Potential infinite loop risk in loadBlock during Analysing phase
  -- loadBlock calls turnOff → many stmt → may recursively call parseFun
  -- If functions have mutual recursion, this could loop forever.
  -- Current mitigation: addFun registers function BEFORE parsing body
  -- (so recursive calls find the function signature in debit system)
  -- TODO: Consider adding max recursion depth check or cycle detection
  let argsWithTypes = zipWith (\(_, t, _) v -> (v, t)) params args
  fun_body <- enterScope *> unifyParams params argsWithTypes *> loadBlock <* quitBlock
  returns <- liftIO $ checkGuaranteedReturn fun_body
  case returns of
    Right False 
      | returnType == TUnit -> pure ()
      | otherwise -> fail $ "* Function \"" ++ funIdent ++ "\" does not guarantee a return value on all code paths"
    Right True -> pure ()
    _ -> fail $ "* Error checking return guarantee in function: " ++ funIdent 

  updateState $ returning .~ rtReturn
  addFunWithToken funIdent funIdent_T (returnType, params, Just fun_body)

  -- Resolve any pending debits for this function (mutual recursion support)
  -- Check if any calls to this function were made before it was declared
  state' <- getState
  case resolveDebits funIdent returnType (map (\(_, t, _) -> t) params) state' of
    Left err -> fail err  -- Signature mismatch - fail with error pointing to call site
    Right newState -> setState newState  -- All calls match - remove debits

  -- Only execute main immediately in Running mode, not other functions
  if isRunning state && funIdent == "main" then
    evalFun funIdent args
  else do
    default_value <- defaultValue returnType
    pure (default_value, returnType) 


-- | Evaluates a function call by binding arguments to parameters and executing the body.
-- Handles parameter unification, return type checking, and state restoration.
-- During Running phase executes body and validates return value; during Analysing returns default value.
-- Example: @evalFun "factorial" [VInt 5]@ binds 5 to parameter, executes body, validates Int return.
evalFun :: Ident -> [Value] -> Interpreter (Value, Typε)
evalFun fun_ident fun_args = do
  mFun <- searchFun fun_ident
  RTState {..} <- getState 

  case mFun of
    Nothing -> fail $ 
      "function didnt found: " 
      ++ fun_ident

    Just (return_type, fun_params, mFunBody) -> do 

      updateState $ returning ?~ return_type

      -- Create ValueInfo from fun_args using parameter types
      let fun_argsWithTypes = zipWith (\(_, t, _) v -> (v, t)) fun_params fun_args
      enterBlock    <* unifyParams fun_params fun_argsWithTypes
      default_value <- defaultValue return_type

      -- | Restores interpreter state flags after function evaluation.
      let restore_state :: Interpreter () = do
            updateState $
              (flag .~ rtFlag) .
              (returning .~ rtReturn)

      -- | Default action during Analysing phase: returns default value without executing body.
      let default_action = do
            quitBlock
            pure (default_value, return_type) 

      -- | Runs function body during Running phase and validates return value type.
      let running_action fun_body = do
            evalBodyInTs fun_body
            quitBlock

            RTState { rtFlag = currentFlag } <- getState
            -- liftIO . print $ "after evaluating the function: "
            --   ++ fun_ident ++ " i am: "
            -- liftIO . print $ show rtFlag

            case currentFlag of 
            -- TODO: falta sempre garantir um retorno
              Returning mValue -> do
                let (returnVal, returnType) = Maybe.fromMaybe (VUnit,TUnit) mValue
                when (returnType /= return_type) $
                  fail $ "type mismatch between "
                    ++ show return_type ++ " (expected by the function "
                    ++ fun_ident  ++ ") and " ++ show returnType

                pure (returnVal, return_type)
  
              _ -> {- liftIO $ print "nao retornando" *> -} pure (VUnit, return_type) 

      maybe default_action running_action mFunBody <* restore_state


-- | Parses dereference assignment: @*p = value@ or @(*p).field = value@.
-- Validates const references and handles optional chains (field access, array indexing).
-- Supports parentheses around dereference: @(*p) = value@.
sDerefAtrib :: Interpreter [Token]
sDerefAtrib = do
  -- Check for optional opening parenthesis
  mOpenParen <- optional Token.openParens
  
  mul_T <- Token.mul
  (varName, var_T) <- Token.varId
  (prefix, VarInfo _ (TRef innerType) constFlag _) <- searchVarInfo varName
  
  when constFlag $ do
    let line = tokenLine var_T
        col = tokenCol var_T
        y = Text.unpack ErrFmt.yellow
        b = Text.unpack ErrFmt.blue
        c = Text.unpack ErrFmt.cyan
        bo = Text.unpack ErrFmt.bold
        r = Text.unpack ErrFmt.reset
    
    liftIO $ TIO.putStrLn $ Text.pack $
      "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0594]: cannot modify immutable reference" ++ r ++ "\n" ++
      "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
      "   " ++ b ++ "|" ++ r ++ "\n" ++
      show line ++ " " ++ b ++ "|" ++ r ++ " *" ++ varName ++ " = ...;\n" ++
      "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " " ++ String.replicate (length varName + 1) '^' ++ r ++ "\n" ++
      "   " ++ b ++ "|" ++ r ++ "\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "reference '" ++ varName ++ "' is declared as const\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "remove " ++ c ++ "const" ++ r ++ " from the reference declaration to make it mutable\n"
    
    fail "* Cannot modify const reference"
  
  -- If we had opening paren, we must have closing paren before the chain
  mCloseParen <- case mOpenParen of
    Just _ -> Just <$> Token.closeParens
    Nothing -> pure Nothing
  
  -- Try to parse chain (e.g., .field or [index]) AFTER the dereferenced variable
  let baseRef = StackRef varName prefix
  ((finalRef, finalType), chainTokens) <- lValueChain (baseRef, innerType)
  
  assign_T <- Token.assign
  ((value, exprType), expr_Ts) <- expr
  
  unless (exprType == finalType) $
    fail $ "* Type mismatch in dereference assignment: expected " ++ show finalType ++ " but got " ++ show exprType
  
  semiColon_T <- Token.semiColon
  
  state <- getState
  when (isRunning state) $ do
    varValue <- searchVarValueAt prefix varName
    case varValue of
      (VRef reference, _) -> do
        -- Apply the chain to the dereferenced reference
        finalRefToWrite <- case chainTokens of
          [] -> pure reference  -- No chain: *ptr = value
          _ -> buildChainRef reference baseRef finalRef  -- With chain: *ptr.field = value
        writeToRef finalRefToWrite value
      (VNull, _actualType) -> 
        fail $ "Runtime Error: Attempting to dereference null pointer '" ++ varName ++ "' at line " ++ show (tokenLine var_T)
      (_actualVal, actualType) -> 
        fail $ "Runtime Error: Cannot dereference non-reference value '" ++ varName ++ "' of type " ++ show actualType ++ " at line " ++ show (tokenLine var_T)
  
  let parenTokens = case (mOpenParen, mCloseParen) of
        (Just openT, Just closeT) -> [openT, mul_T, var_T, closeT]
        _ -> [mul_T, var_T]
  
  pure $ parenTokens ++ chainTokens ++ [assign_T] ++ expr_Ts ++ [semiColon_T]
  where
    -- Build the actual reference chain from the dereferenced base
    buildChainRef :: Ref -> Ref -> Ref -> Interpreter Ref
    buildChainRef actualRef expectedBase finalRef = 
      case finalRef of
        StackRef _ _ -> pure actualRef  -- Base case
        FieldRef baseRef fieldName -> do
          parentRef <- buildChainRef actualRef expectedBase baseRef
          pure (FieldRef parentRef fieldName)
        IndexRef baseRef idx -> do
          parentRef <- buildChainRef actualRef expectedBase baseRef
          pure (IndexRef parentRef idx)
        HeapRef _ -> pure actualRef  -- Heap reference case


-- Field access-- | Disambiguates identifier-based statements (assignment vs function call).
-- First attempts to parse as assignment with optional chain (@x.field = value@),
-- then falls back to function call if that fails.
sIdentBasedStmt :: Interpreter [Token]
sIdentBasedStmt = do
  (ident, ident_T) <- Token.varId

  choice
    [ finishFunCall ident ident_T =<< Token.openParens 
    , do
        (prefix, VarInfo _ varType _ _) <- searchVarInfo ident
        let baseRef = StackRef ident prefix
        ((finalRef, finalType), chainTokens) <- lValueChain (baseRef, varType)
        assign_T <- Token.assign
        finishAtribWithChain ident ident_T finalRef finalType chainTokens assign_T
    ]
      

-- | Completes assignment with optional chain (field access or array indexing).
-- Validates const variables and type compatibility, then writes value through reference chain.
-- Example: @finishAtribWithChain "p" token ref TInt [".x"] assignToken@ assigns to p.x.
finishAtribWithChain :: Ident -> Token -> Ref -> Typε -> [Token] -> Token -> Interpreter [Token]
finishAtribWithChain varIdent varIdent_T finalRef finalType chainTokens assign_T = do
  (_, VarInfo _ _ constFlag _) <- searchVarInfo varIdent
  
  -- Se não tem chain e é const, falha
  when (null chainTokens && constFlag) $ do
    let line = tokenLine varIdent_T
        col = tokenCol varIdent_T
        y = Text.unpack ErrFmt.yellow
        b = Text.unpack ErrFmt.blue
        c = Text.unpack ErrFmt.cyan
        bo = Text.unpack ErrFmt.bold
        r = Text.unpack ErrFmt.reset
    
    liftIO $ TIO.putStrLn $ Text.pack $
      "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0594]: cannot modify immutable variable" ++ r ++ "\n" ++
      "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
      "   " ++ b ++ "|" ++ r ++ "\n" ++
      show line ++ " " ++ b ++ "|" ++ r ++ " " ++ varIdent ++ " = ...;\n" ++
      "   " ++ b ++ "|" ++ r ++ " " ++ y ++ String.replicate (length varIdent) '^' ++ r ++ "\n" ++
      "   " ++ b ++ "|" ++ r ++ "\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "variable '" ++ varIdent ++ "' is declared as const\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "remove " ++ c ++ "const" ++ r ++ " from the variable declaration: " ++ 
      c ++ "var " ++ varIdent ++ ": ... = ...;" ++ r ++ "\n"
    
    fail "* Cannot modify const variable"
  
  -- Parseia a expressão do lado direito
  ((value, exprType), expr_Ts) <- expr

  unless (exprType == finalType) $
    fail $ "* Type mismatch in assignment: expected " ++ show finalType ++ " but got " ++ show exprType

  semiColon_T <- Token.semiColon

  state <- getState
  when (isRunning state) $ do
    writeToRef finalRef value

  pure $ [varIdent_T] ++ chainTokens ++ [assign_T] ++ expr_Ts ++ [semiColon_T]

-- | Completes function call statement parsing after the opening parenthesis.
-- Validates that function returns Unit (is a procedure), parses arguments, and evaluates in Running mode.
-- Example: @finishFunCall "print" identToken openParenToken@ parses arguments and calls print.
finishFunCall :: Ident -> Token -> Token -> Interpreter [Token]
finishFunCall fun_ident fun_ident_T openParens_T = do
  (args, args_Ts) <- (`sepBy'` Token.comma) expr
  closeParens_T   <- Token.closeParens

  semiColon_T <- Token.semiColon
    
  let tokens = [fun_ident_T, openParens_T] 
        ++ args_Ts ++ [closeParens_T]
        ++ [semiColon_T]

  mFun <- searchFun fun_ident
  
  (return_type, fun_params) <- case mFun of
    Just (retType, params, _) -> pure (retType, params)
    Nothing -> do
      -- Function not found - support forward reference
      state <- getState
      if isRunning state then
        -- In Running phase, function must exist
        compileError fun_ident_T "Undefined function" $
          "Function '" ++ fun_ident ++ "' is not declared in the current scope.\n" ++
          "help: Make sure the function is defined before calling it"
      else do
        -- In Analysing phase, add debit for later resolution
        -- Assume Unit return type for procedure calls
        let paramTypes = fmap snd args
        let callSig = CallSignature 
              { callName = fun_ident
              , callTokens = tokens
              , callParams = paramTypes
              , callReturnType = TUnit
              }
        modifyState $ addDebit fun_ident callSig
        
        -- Return placeholder for analysis
        let placeholderParams = fmap (\(_, t) -> ("", t, False)) args
        pure (TUnit, placeholderParams)

  -- Warn if calling function that returns value as statement (ignoring return)
  when (return_type /= TUnit) $ do
    let line = tokenLine fun_ident_T
        col = tokenCol fun_ident_T
        typeStr = case return_type of
          TInt -> "int"
          TFloat -> "float"
          TText -> "text"
          TBool -> "bool"
          TCustom name -> name
          TDynArray t -> "[" ++ show t ++ "]"
          TRef t -> "&" ++ show t
          _ -> show return_type
        y = Text.unpack ErrFmt.yellow
        b = Text.unpack ErrFmt.blue
        c = Text.unpack ErrFmt.cyan
        bo = Text.unpack ErrFmt.bold
        r = Text.unpack ErrFmt.reset
    
    liftIO $ TIO.putStrLn $ Text.pack $ 
      "\n" ++ y ++ "warning" ++ r ++ bo ++ ": unused return value" ++ r ++ "\n" ++
      "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
      "   " ++ b ++ "|" ++ r ++ "\n" ++
      show line ++ " " ++ b ++ "|" ++ r ++ " " ++ fun_ident ++ "(...);\n" ++
      "   " ++ b ++ "|" ++ r ++ " " ++ y ++ String.replicate (length fun_ident) '^' ++ r ++ "\n" ++
      "   " ++ b ++ "|" ++ r ++ "\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ 
        "function '" ++ fun_ident ++ "' returns " ++ bo ++ typeStr ++ r ++ " but the value is discarded\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ 
        "assign the result to a variable: " ++ c ++ "var result: " ++ typeStr ++ " = " ++ fun_ident ++ "(...);" ++ r ++ "\n" ++
      "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ 
        "or change the function to return Unit (procedure): " ++ c ++ "fn " ++ fun_ident ++ "(...) { ... }" ++ r ++ "\n"

  state <- getState
  if isRunning state then do
    void $ evalFun fun_ident $ fmap fst args

  else do
    enterBlock    <* unifyParams fun_params args *> quitBlock

  pure tokens

-- | Recursively builds reference chains for l-values (field access and array indexing).
-- Parses optional chains like @.field[index].anotherField@ and validates types at each step.
-- During Running phase validates array indices; during Analysing uses placeholder (0).
-- Example: @lValueChain (StackRef "p" 0, TCustom "Point")@ with @.x[5]@ returns @IndexRef (FieldRef ... "x") 5@.
lValueChain :: (Ref, Typε) -> Interpreter ((Ref, Typε), [Token])
lValueChain (current_ref, current_type) = do
  -- Color codes for professional error messages
  let y = Text.unpack ErrFmt.yellow
      b = Text.unpack ErrFmt.blue
      c = Text.unpack ErrFmt.cyan
      bo = Text.unpack ErrFmt.bold
      r = Text.unpack ErrFmt.reset

  mDot <- optional Token.dot
  case mDot of
    Just dot_T -> do
       (field, field_T) <- Token.varId

       field_type <- case current_type of
          TCustom sName -> do
             typeDef <- searchStruct sName 
             case typeDef of
                DStruct _ fieldsMap -> 
                   case Map.lookup field fieldsMap of
                      Just t -> pure t
                      Nothing -> do
                        let line = tokenLine field_T
                            col = tokenCol field_T
                            availableFields = Map.keys fieldsMap
                        
                        liftIO $ TIO.putStrLn $ Text.pack $
                          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0609]: no field '" ++ field ++ "' on type '" ++ sName ++ "'" ++ r ++ "\n" ++
                          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
                          "   " ++ b ++ "|" ++ r ++ "\n" ++
                          show line ++ " " ++ b ++ "|" ++ r ++ " struct." ++ field ++ "\n" ++
                          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "       " ++ String.replicate (length field) '^' ++ r ++ "\n" ++
                          "   " ++ b ++ "|" ++ r ++ "\n" ++
                          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "field '" ++ field ++ "' does not exist in struct '" ++ sName ++ "'\n" ++
                          (if null availableFields 
                           then ""
                           else "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "available fields: " ++ 
                                c ++ List.intercalate ", " availableFields ++ r ++ "\n") ++
                          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "check the field name spelling\n"
                        
                        fail $ "FieldRef '" ++ field ++ "' not found in struct '" ++ sName ++ "'"
          _ -> do
            let line = tokenLine dot_T
            let col = tokenCol dot_T
            liftIO $ TIO.putStrLn $ Text.pack $
              "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0609]: dot access requires a struct" ++ r ++ "\n" ++
              "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
              "   " ++ b ++ "|" ++ r ++ "\n" ++
              show line ++ " " ++ b ++ "|" ++ r ++ " <expr>." ++ field ++ "\n" ++
              "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "^^^^^" ++ r ++ " " ++ c ++ "expected struct type here" ++ r ++ "\n" ++
              "   " ++ b ++ "|" ++ r ++ "\n" ++
              "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "the left side of '.' must be a struct expression\n" ++
              "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ show current_type ++ " instead\n" ++
              "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "ensure you are accessing fields only on struct values\n"
            fail "Type Error: Dot access expects a struct."

       let new_ref = FieldRef current_ref field
       let tokens = [dot_T, field_T]

       ((finalRef, finalType), nextTokens) <- lValueChain (new_ref, field_type)
       pure ((finalRef, finalType), tokens ++ nextTokens)

    Nothing -> do
      maybeBracket <- optional Token.openBracket
      case maybeBracket of
        Just open_T -> do
           ((vIndex, tIndex), indexTokens) <- expr
           close_T <- Token.closeBracket
           
           state <- getState
           
           -- Descobre o tipo interno do array
           innerType <- case current_type of
              TDynArray t -> pure t
              _ -> do
                let line = tokenLine open_T
                let col = tokenCol open_T
                liftIO $ TIO.putStrLn $ Text.pack $
                  "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: cannot index into non-array type" ++ r ++ "\n" ++
                  "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
                  "   " ++ b ++ "|" ++ r ++ "\n" ++
                  show line ++ " " ++ b ++ "|" ++ r ++ " value[index]\n" ++
                  "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "^^^^^" ++ r ++ " " ++ c ++ "type: " ++ show current_type ++ r ++ "\n" ++
                  "   " ++ b ++ "|" ++ r ++ "\n" ++
                  "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "array indexing with [] only works on array types\n" ++
                  "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ show current_type ++ " which is not an array\n" ++
                  "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "ensure the value is an array before indexing\n"
                fail $ "Type Error: Cannot index type " ++ show current_type
           
           -- Durante execução, validar e usar o índice
           -- Durante type checking, usar 0 como placeholder
           indexVal <- if isRunning state then
              case (vIndex, tIndex) of
                 (VInt i, TInt) -> pure i
                 _ -> do
                   let indexTokens_hd = if null indexTokens then open_T else case indexTokens of (h:_) -> h; [] -> open_T
                   let line = tokenLine indexTokens_hd
                   let col = tokenCol indexTokens_hd
                   liftIO $ TIO.putStrLn $ Text.pack $
                     "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: array index must be an integer" ++ r ++ "\n" ++
                     "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
                     "   " ++ b ++ "|" ++ r ++ "\n" ++
                     show line ++ " " ++ b ++ "|" ++ r ++ " arr[index]\n" ++
                     "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "    ^^^^^" ++ r ++ " " ++ c ++ "type: " ++ show tIndex ++ r ++ "\n" ++
                     "   " ++ b ++ "|" ++ r ++ "\n" ++
                     "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "array indices must be of type Int\n" ++
                     "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "found type " ++ show tIndex ++ " instead\n" ++
                     "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "convert the index expression to Int or use an integer variable\n"
                   fail $ "Type Error: Array index must be Int, got " ++ show tIndex ++ "."
           else
              pure 0  -- Placeholder durante type checking

           let newRef = IndexRef current_ref indexVal
           let tokens = [open_T] ++ indexTokens ++ [close_T]

           -- Recursão
           ((finalRef, finalType), nextTokens) <- lValueChain (newRef, innerType)
           pure ((finalRef, finalType), tokens ++ nextTokens)

        Nothing -> 
           pure ((current_ref, current_type), [])

-- | Parses reference creation expression: @&variable@ or @&variable.field[index]@.
-- Creates a reference to an l-value (variable, field, or array element).
-- Uses 'lValueChain' to handle nested access patterns.
-- Example: @eRef@ parsing @&p.x@ returns @(VRef (FieldRef (StackRef "p" 0) "x"), TRef TInt)@.
eRef :: Interpreter (ValueInfo, [Token])
eRef = do
  amp_T <- Token.ampersand 
  
  (varName, var_T) <- Token.varId
  
  (prefix, VarInfo _ varType _ _) <- searchVarInfo varName
  
  let baseRef = StackRef varName prefix
  
  ((finalRef, finalType), chainTokens) <- lValueChain (baseRef, varType)
  
  let tokens = amp_T : var_T : chainTokens
  
  pure ((VRef finalRef, TRef finalType), tokens)

-- | Recursively resolves a reference to its actual value during Running phase.
-- Handles stack variables, heap addresses, field access, and array indexing.
-- Returns the final value and type after following all reference chains.
-- Example: @resolveRef (FieldRef (StackRef "p" 0) "x")@ retrieves value of p.x.
resolveRef :: Ref -> Interpreter ValueInfo
resolveRef reference = case reference of
  StackRef ident prefix -> 
    searchVarValueAt prefix ident

  HeapRef addr -> do
    currentHeap <- view heap <$> getState
    case Map.lookup addr currentHeap of
      Nothing -> do
        let y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E1003]: invalid heap reference (use-after-free)" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<runtime>\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "attempted to access memory address " ++ show addr ++ " which has been freed\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "this typically happens when:\n" ++
          "   " ++ b ++ "=" ++ r ++ "        - a pointer is used after the memory it points to has been deallocated\n" ++
          "   " ++ b ++ "=" ++ r ++ "        - a reference is kept to data that went out of scope\n" ++
          "   " ++ b ++ "=" ++ r ++ "        - the heap is corrupted\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "ensure all pointers remain valid during their lifetime\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "avoid keeping references to temporary values\n"
        fail $ "Runtime Error: Invalid heap reference (use after free)"
      Just valueInfo -> pure valueInfo

  FieldRef baseRef fieldIdent -> do
    (baseVal, baseType) <- resolveRef baseRef
    case (baseVal, baseType) of
      (VStruct _ fields, TCustom sIdent) -> 
        case Map.lookup fieldIdent fields of
          Nothing -> do
            let y = Text.unpack ErrFmt.yellow
                b = Text.unpack ErrFmt.blue
                c = Text.unpack ErrFmt.cyan
                bo = Text.unpack ErrFmt.bold
                r = Text.unpack ErrFmt.reset
                availableFields = Map.keys fields
            liftIO $ TIO.putStrLn $ Text.pack $
              "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0609]: field not found in struct" ++ r ++ "\n" ++
              "  " ++ b ++ "--> " ++ r ++ "<runtime>\n" ++
              "   " ++ b ++ "|" ++ r ++ "\n" ++
              "   " ++ b ++ "|" ++ r ++ " struct." ++ fieldIdent ++ "\n" ++
              "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "       " ++ String.replicate (length fieldIdent) '^' ++ r ++ "\n" ++
              "   " ++ b ++ "|" ++ r ++ "\n" ++
              "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "field '" ++ c ++ fieldIdent ++ r ++ "' does not exist in struct '" ++ c ++ sIdent ++ r ++ "'\n" ++
              (if null availableFields
               then ""
               else "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "available fields: " ++ c ++ List.intercalate ", " availableFields ++ r ++ "\n") ++
              "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "check the field name spelling or struct definition\n"
            fail $ "Runtime Error: FieldRef '" ++ fieldIdent ++ "' not found in struct of type " ++ sIdent ++ "."
          Just (mVal, fType) -> do
            val <- maybe (defaultValue fType) pure mVal
            pure (val, fType)
      (_actualVal, actualType) -> do
        let y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0609]: cannot access field of non-struct" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<runtime>\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ " value." ++ fieldIdent ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^^" ++ r ++ " " ++ c ++ "type: " ++ show actualType ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "attempted to access field '" ++ c ++ fieldIdent ++ r ++ "' on non-struct value\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "the value has type " ++ c ++ show actualType ++ r ++ " but expected a struct\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "ensure the value is a struct before accessing fields\n"
        fail $ "Runtime Error: Accessing field '" ++ fieldIdent ++ "' of non-struct value. Got type: " ++ show actualType ++ "."

  IndexRef baseRef idx -> do
    (baseVal, baseType) <- resolveRef baseRef
    case (baseVal, baseType) of
      (VDynArray vec, TDynArray elemType) ->
        case vec Vector.!? fromIntegral idx of
          Nothing -> do
            let y = Text.unpack ErrFmt.yellow
                b = Text.unpack ErrFmt.blue
                c = Text.unpack ErrFmt.cyan
                bo = Text.unpack ErrFmt.bold
                r = Text.unpack ErrFmt.reset
                arrLen = Vector.length vec
            liftIO $ TIO.putStrLn $ Text.pack $
              "\n" ++ y ++ "error" ++ r ++ bo ++ "[E1004]: array index out of bounds" ++ r ++ "\n" ++
              "  " ++ b ++ "--> " ++ r ++ "<runtime>\n" ++
              "   " ++ b ++ "|" ++ r ++ "\n" ++
              "   " ++ b ++ "|" ++ r ++ " array[" ++ show idx ++ "]\n" ++
              "   " ++ b ++ "|" ++ r ++ " " ++ y ++ "      " ++ String.replicate (length (show idx)) '^' ++ r ++ "\n" ++
              "   " ++ b ++ "|" ++ r ++ "\n" ++
              "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "index " ++ c ++ show idx ++ r ++ " is out of bounds\n" ++
              "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "array has length " ++ c ++ show arrLen ++ r ++ " (valid indices: 0.." ++ show (arrLen - 1) ++ ")\n" ++
              "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "check the array length before indexing\n" ++
              "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "add bounds checking: if (index >= 0 && index < length) { ... }\n"
            fail $ "Runtime Error: IndexRef " ++ show idx ++ " out of bounds for array of length " ++ show (Vector.length vec) ++ "."
          Just val -> pure (val, elemType)
      (_actualVal, actualType) -> do
        let y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E0308]: cannot index non-array type" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<runtime>\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ " value[index]\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^^" ++ r ++ " " ++ c ++ "type: " ++ show actualType ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "attempted to index a non-array value\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "the value has type " ++ c ++ show actualType ++ r ++ " but expected an array\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "ensure the value is an array before indexing\n"
        fail $ "Runtime Error: Indexing non-array value. Got type: " ++ show actualType ++ "."

-- | Parses dereference operator: @*expr@.
-- During Running phase resolves reference to actual value; during Analysing uses placeholder.
-- Handles null pointer checking at runtime (fails with error).
-- Example: @eDeref mulToken refExpr@ where refExpr is @&x@ returns value of x.
eDeref :: Token -> InterPerator -> InterPerator
eDeref token iE = do
  ((val, typ), expr_Ts) <- iE
  
  case (val, typ) of
    -- Null dereferencing results in runtime error
    (VNull, TRef innerType) -> do
      state <- getState
      if isRunning state then do
        let line = tokenLine token
            col = tokenCol token
            y = Text.unpack ErrFmt.yellow
            b = Text.unpack ErrFmt.blue
            c = Text.unpack ErrFmt.cyan
            bo = Text.unpack ErrFmt.bold
            r = Text.unpack ErrFmt.reset
        
        liftIO $ TIO.putStrLn $ Text.pack $
          "\n" ++ y ++ "error" ++ r ++ bo ++ "[E1002]: null pointer dereference" ++ r ++ "\n" ++
          "  " ++ b ++ "--> " ++ r ++ "<source>:" ++ show line ++ ":" ++ show col ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          show line ++ " " ++ b ++ "|" ++ r ++ " *null_ptr\n" ++
          "   " ++ b ++ "|" ++ r ++ " " ++ y ++ " ^^^^^^^^^" ++ r ++ "\n" ++
          "   " ++ b ++ "|" ++ r ++ "\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "note: " ++ r ++ "attempting to dereference a null pointer causes a runtime panic\n" ++
          "   " ++ b ++ "=" ++ r ++ " " ++ bo ++ "help: " ++ r ++ "check if the pointer is null before dereferencing: " ++ 
          c ++ "if (ptr != null) { ... }" ++ r ++ "\n"
        
        fail "Runtime Error: Attempting to dereference null pointer"
      else do
        -- Use placeholder value during analysis phase instead of defaultValue
        let v = placeholderValue innerType
        pure ((v, innerType), token : expr_Ts)
    
    (VRef reference, TRef innerType) -> do
  
      state <- getState
      if isRunning state then do
        (targetVal, targetType) <- resolveRef reference
        pure ((targetVal, targetType), token : expr_Ts)

      else do
        -- Use placeholder value during analysis phase instead of defaultValue
        let v = placeholderValue innerType
        pure ((v, innerType), token : expr_Ts)
      
    (_, otherT) -> 
      fail $ "Type Error: Expected a Reference to dereference, but got " ++ show otherT

-- | Writes a value through a reference during Running phase.
-- Handles all reference types: stack variables, heap addresses, struct fields, array elements.
-- Recursively updates parent structures when writing to fields or array indices.
-- Example: @writeToRef (FieldRef (StackRef "p" 0) "x") (VInt 42)@ sets p.x to 42.
writeToRef :: Ref -> Value -> Interpreter ()
writeToRef ref newValue = case ref of
  StackRef ident prefix -> 
    if prefix == -1
      then updateGlobalVar ident newValue  -- Global variable
      else updateVarAt prefix ident newValue  -- Local variable

  HeapRef addr -> do
    currentHeap <- view heap <$> getState
    case Map.lookup addr currentHeap of
      Nothing -> fail $ "Runtime Error: Invalid heap reference (use after free)"
      Just (_, typε) -> do
        -- Update heap with new value, preserving type
        modifyState $ over heap (Map.insert addr (newValue, typε))

  FieldRef baseRef fieldName -> do
    (parentVal, _parentType) <- resolveRef baseRef 

    case parentVal of
      VStruct sName fields -> do
        -- Obtém o tipo do campo para verificar
        case Map.lookup fieldName fields of
          Just (_, fieldType) -> do
            let newFields = Map.insert fieldName (Just newValue, fieldType) fields
            let newStruct = VStruct sName newFields
            writeToRef baseRef newStruct
          Nothing -> fail $ "* Runtime Error: FieldRef '" ++ fieldName ++ "' not found in struct"

      _ -> fail "* Runtime Error: Trying to set field of non-struct"

  IndexRef baseRef idx -> do
    (parentVal, _parentType) <- resolveRef baseRef

    case parentVal of
      VDynArray vec -> do
        -- Verifica limites
        when (idx < 0 || idx >= toInteger (Vector.length vec)) $ 
          fail $ "* Runtime Error: IndexRef " ++ show idx ++ " out of bounds (array size: " ++ show (Vector.length vec) ++ ")"

        let newVec = vec Vector.// [(fromInteger idx, newValue)]
        let newArray = VDynArray newVec

        writeToRef baseRef newArray

      _ -> fail "* Runtime Error: Trying to index non-array"
--
--
--
--
--
--
--
--
--
--
--
--
--
