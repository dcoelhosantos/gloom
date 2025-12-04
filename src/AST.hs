{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : AST
Description : Abstract Syntax Tree for the Gloom programming language
Copyright   : (c) João, 2024-2025
License     : MIT
Maintainer  : jjoaoll@example.com
Stability   : experimental

This module defines the complete Abstract Syntax Tree (AST) for Gloom,
a statically-typed imperative language with heap allocation, references,
and two-phase execution model (Analysing + Running).

= Design-- ===================================================================
-- 6. MELHORIA: Instância 'Show' bonita para Expr
-- ===================================================================

-- | Format function arguments or constructor arguments as @(arg1, arg2, ...)@
showArgs :: [Expr] -> String
showArgs args = "(" ++ List.intercalate ", " (fmap show args) ++ ")"

-- | Add parentheses to sub-expressions when needed for clarity
--
-- Leaves simple expressions (variables, literals, field access) unparenthesized.
-- Adds parentheses to complex expressions to avoid ambiguity like @a + b * c@.
showParenExpr :: Expr -> String
showParenExpr e@(EVar _)         = show e
showParenExpr e@(ELit _)         = show e
showParenExpr e@(EComp _ _ [])   = show e -- Construtor sem args
showParenExpr e@(EFunCall _ [])  = show e -- Chamada sem args
showParenExpr e@(EArrayAccess {}) = show e
showParenExpr e@(EFieldAccess {}) = show e
showParenExpr e@(EDynArray _)     = show e
-- Adiciona parênteses no resto para garantir
showParenExpr e                  = "(" ++ show e ++ ")"mbraces /controlled pessimism/ - like Rust, but for people who don't
want to rewrite their code 47 times. We have:

* Memory safety through heap allocation (@new@) and explicit deallocation (@drop@)
* Compile-time type checking (Analysing phase) + Runtime execution (Running phase)
* References (@&T@) and dereferencing (@*ref@) with null safety checks
* Struct types with field access, including nested dereferencing: @(*ptr).field@

= Example Usage

@
-- See code-examples/01-basics/hello-world.gloom
fn main() {
    print("Hello from Gloom!");
}
@

For more examples, check the @code-examples/@ directory in the source tree.
-}
module AST
  ( -- * Identifiers and Basic Types
    Ident

    -- * Type System
  , Typε(..)
  , Value(..)
  , HeapAddr
  , Ref(..)
  , Fields
  , TypeDef(..)

    -- * Expressions
  , Expr(..)
  , BinOp(..)
  , UnOp(..)
  , Args
  ) where

-- global
import Data.Vector hiding ((++))

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text

-- | Identifier type - represents variable names, function names, struct names, etc.
-- 
-- We use a simple String alias for now, but this could be upgraded to a newtype
-- with smart constructors for better type safety.
type Ident = String

-- | Code blocks consist of statements and an optional return expression
--
-- Similar to Rust, the last expression in a block can be its return value.
-- Example from @code-examples/04-functions/function-bool-return.gloom@:
--
-- @
-- fn is_positive(x: int) -> bool {
--     return x > 0;
-- }
-- @

-- | Types in the Gloom type system
--
-- Gloom has a simple but expressive type system with primitives, user-defined
-- types (structs/variants), arrays, and references.
--
-- = Primitive Types
-- * 'TBool' - Boolean values (@true@ \/ @false@)
-- * 'TInt' - Integer numbers
-- * 'TFloat' - Floating-point numbers  
-- * 'TText' - UTF-8 text strings
-- * 'TUnit' - The unit type (void), used for functions with no return value
--
-- = Composite Types
-- * 'TCustom' - User-defined structs and variants
-- * 'TDynArray' - Dynamically-sized arrays: @[int]@, @[Point]@, etc.
-- * 'TRef' - References to values: @&int@, @&Point@, etc.
--
-- = Special Types  
-- * 'TAny' - Used internally during type inference
--
-- == Examples
--
-- @
-- var x: int = 42;              -- TInt
-- var names: [text];            -- TDynArray TText
-- var ptr: &Point;              -- TRef (TCustom "Point")
-- const enabled: bool = true;   -- TBool (must be initialized!)
-- @
--
-- See @code-examples/02-types/@ for more type examples.
data Typε 
  = TBool
  | TInt
  | TFloat
  | TText
  | TUnit
  | TCustom Ident   -- ^ User-defined struct or variant type

  | TDynArray Typε  -- ^ Dynamic array of elements
  | TRef Typε       -- ^ Reference (pointer) to a value

  | TAny            -- ^ Wildcard type for inference
  deriving (Ord)

instance Eq Typε where 
  TBool == TBool     = True
  TInt == TInt       = True
  TFloat == TFloat   = True
  TText == TText     = True
  TUnit == TUnit     = True
  TCustom ident0 == TCustom ident1 = 
    ident0 == ident1

  TDynArray typε0 == TDynArray typε1 = 
    typε0 == typε1

  TRef typε0 == TRef typε1 = 
    typε0 == typε1

  TAny == _     = True
  _    == TAny = True

  _ == _ = False

-- ===================================================================
-- 2. MELHORIA: Instância 'Show' bonita para Typε
-- ===================================================================
instance Show Typε where
  show TBool         = "bool"
  show TInt          = "int"
  show TFloat        = "float"
  show TText         = "text"
  show TUnit         = "void"
  show (TCustom i)   = show i -- Usa o 'Show' limpo de Ident
  show (TDynArray t) = "[" ++ show t ++ "]"
  show (TRef t)      = "&" ++ show t
  show TAny          = "any"

-- | Runtime values in the Gloom interpreter
--
-- Values are the actual data computed and stored during program execution.
-- Each 'Value' corresponds to a 'Typε' in the type system.
--
-- = Primitive Values
-- * 'VBool' - Boolean: @true@ or @false@
-- * 'VInt' - Integer: @42@, @-17@, @0@
-- * 'VFloat' - Floating-point: @3.14@, @-0.5@
-- * 'VText' - Unicode text: @\"hello\"@, @\"Gloom\"@
-- * 'VUnit' - Unit value (like @void@ in C)
--
-- = Special Values
-- * 'VNull' - Null pointer\/reference (checked at runtime!)
-- * 'VDynArray' - Dynamic array stored as Vector
-- * 'VStruct' - Struct instance with fields
-- * 'VRef' - Reference to a location (stack or heap)
--
-- == Examples
--
-- @
-- VInt 42                              -- integer literal
-- VBool True                           -- boolean
-- VText "hello"                        -- text string
-- VDynArray (fromList [VInt 1, VInt 2]) -- array
-- VNull                                -- null reference
-- VRef (StackRef "x" 0)                -- reference to variable x
-- @
--
-- See @code-examples/11-heap/@ for heap allocation and reference examples.
data Value
  = VBool Bool
  | VInt Integer 
  | VFloat Double
  | VText Text.Text
  | VUnit 
  | VNull  -- ^ Null reference\/pointer value
  | VDynArray (Vector Value)
  | VStruct Ident (Map.Map Ident (Maybe Value, Typε))  -- ^ Struct instance with fields
  | VRef Ref  -- ^ Reference to a memory location
  deriving (Eq) 

-- | Heap address type for dynamic allocation
--
-- Used by 'HeapRef' to identify dynamically allocated values.
-- Addresses are assigned sequentially starting from 1.
type HeapAddr = Integer

-- | Memory references in Gloom
--
-- References can point to stack variables, heap-allocated memory,
-- array elements, or struct fields. They support chaining for
-- complex access patterns like @(*ptr).field[index]@.
--
-- = Reference Types
-- * 'StackRef' - Points to a stack variable by name and scope
-- * 'HeapRef' - Points to heap memory by address
-- * 'IndexRef' - Array element access: @ref[index]@
-- * 'FieldRef' - Struct field access: @ref.field@
--
-- == Examples
--
-- @
-- StackRef "x" 0           -- variable x in global scope
-- HeapRef 42               -- heap address 42
-- IndexRef (StackRef "arr" 0) 5   -- arr[5]
-- FieldRef (StackRef "p" 1) "x"   -- p.x
-- @
--
-- References are created with @&@ and dereferenced with @*@.
-- See @code-examples/05-pointers-refs/@ and @code-examples/09-references-tests/@.
data Ref 
  = StackRef Ident Integer  -- ^ Stack reference: variable name + scope prefix
  | HeapRef HeapAddr        -- ^ Heap reference: address in dynamic memory
  | IndexRef Ref Integer    -- ^ Array indexing (works on both stack and heap)
  | FieldRef Ref Ident      -- ^ Field access (works on both stack and heap)
  deriving (Eq, Ord)

-- | C-style pointer formatting
instance Show Ref where
  show (StackRef ident offset) = ident ++ " (stack offset " ++ show offset ++ ")"
  show (HeapRef addr) = "0x" ++ showHex addr ""
  show (IndexRef ref idx) = show ref ++ "[" ++ show idx ++ "]"
  show (FieldRef ref field) = show ref ++ "." ++ field

-- Helper for hex formatting
showHex :: Integer -> String -> String
showHex n acc
  | n == 0 = if Prelude.null acc then "0" else acc
  | otherwise = showHex (n `div` 16) (hexDigit (n `mod` 16) : acc)
  where
    hexDigit d
      | d < 10 = toEnum (fromEnum '0' + fromIntegral d)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral (d - 10))

--------------------------
instance Ord Value where
    (VBool bL)   `compare` (VBool bR)   = bL `compare` bR
    (VInt nL)    `compare` (VInt nR)    = nL `compare` nR
    (VFloat xL)  `compare` (VFloat xR)  = xL `compare` xR 
    (VText tL)   `compare` (VText tR)   = tL `compare` tR
    VUnit        `compare` VUnit        = EQ
    VNull        `compare` VNull        = EQ
    (VDynArray aL) `compare` (VDynArray aR) = aL `compare` aR 
    -- (VComp iL vL) `compare` (VComp iR vR) = (iL, vL) `compare` (iR, vR) 
    -- (VRef _)      `compare` (VRef _)     = EQ
    -- VBool < VInt < VFloat < VText < VUnit < VNull < VDynArray < VComp < VRef
    
    (VBool _)  `compare` _           = Prelude.LT
    _          `compare` (VBool _)   = Prelude.GT

    (VInt _)   `compare` _           = Prelude.LT
    _          `compare` (VInt _)    = Prelude.GT
    
    (VFloat _) `compare` _           = Prelude.LT
    _          `compare` (VFloat _)  = Prelude.GT

    (VText _)  `compare` _           = Prelude.LT
    _          `compare` (VText _)   = Prelude.GT

    VUnit      `compare` _           = Prelude.LT
    _          `compare` VUnit       = Prelude.GT

    VNull      `compare` _           = Prelude.LT
    _          `compare` VNull       = Prelude.GT

    (VDynArray _) `compare` _        = Prelude.LT
    _             `compare` (VDynArray _) = Prelude.GT

    -- (VComp _ _) `compare` _         = Prelude.LT
    -- _           `compare` (VComp _ _) = Prelude.GT
    _ `compare` _ = Prelude.LT
--------------------------


instance Show Value where
  show (VBool b)     = show b
  show (VInt i)      = show i
  show (VFloat x)    = show x
  show (VText text)  = show text
  show VUnit         = "void"
  show VNull         = "null"
  show (VDynArray vs) = show $ toList vs
  show (VRef ref) =  "&" ++ show ref
  show (VStruct ident fields) = showStructIndented 0 ident fields

-- | Show struct with proper indentation for nested structures
showStructIndented :: Int -> Ident -> Map.Map Ident (Maybe Value, Typε) -> String
showStructIndented level ident fields =
  let indent = Prelude.replicate (level * 4) ' '
      fieldIndent = Prelude.replicate ((level + 1) * 4) ' '
      header = indent ++ show ident ++ " {"
      fields' = Map.toList $ fmap (\(mVal, _typ) -> mVal) fields
      
      prettyField (fieldIdent, mValue) =
        fieldIndent ++ show fieldIdent ++ ": " ++ formatValue (level + 1) mValue
      
      formatValue :: Int -> Maybe Value -> String
      formatValue _ Nothing = "null"
      formatValue lvl (Just (VStruct sIdent sFields)) = 
        "\n" ++ showStructIndented lvl sIdent sFields
      formatValue _ (Just val) = show val
      
      fieldStrings = fmap prettyField fields'
  in
  if List.null fieldStrings
  then header ++ " }"
  else header ++ "\n" ++ List.intercalate ",\n" fieldStrings ++ "\n" ++ indent ++ "}"

-- | Field name to type mapping for struct definitions
type Fields = Map.Map Ident Typε

-- | User-defined type definitions (structs only)
--
-- Gloom supports struct types:
--
-- = Structs (Product Types)
--
-- Structs are records with named fields:
--
-- @
-- struct Point {
--   x: int,
--   y: int
-- }
--
-- var p: Point = Point { x: 10, y: 20 };
-- print(p.x);  -- Field access
-- @
--
-- See @code-examples/02-types/@ for examples.
data TypeDef
  = DStruct  Ident Fields -- ^ Struct definition: name + fields
  deriving (Eq, Ord)

-- | Expression arguments (for function calls and constructors)
type Args = [Expr]

-- | Expressions - produce values
--
-- Expressions are evaluated to produce runtime values.
-- Unlike statements, they always have a type and produce a result.
--
-- = Literals and Variables
--
-- * 'EVar' - Variable reference
-- * 'ELit' - Literal value (int, bool, text, etc.)
--
-- @
-- x                 -- EVar "x"
-- 42                -- ELit (VInt 42)
-- "hello"           -- ELit (VText "hello")
-- true              -- ELit (VBool True)
-- @
--
-- = Function Calls and Constructors
--
-- * 'EFunCall' - Function call with arguments
-- * 'EComp' - Variant constructor call
--
-- @
-- factorial(5)                    -- EFunCall "factorial" [ELit (VInt 5)]
-- Option::Some(42)                -- EComp "Option" "Some" [ELit (VInt 42)]
-- @
--
-- = Operators
--
-- * 'EUnOp' - Unary operators (-, !)
-- * 'EBinOp' - Binary operators (+, *, ==, &&, etc.)
--
-- @
-- -x                -- EUnOp Neg (EVar "x")
-- !flag             -- EUnOp Not (EVar "flag")
-- a + b             -- EBinOp (EVar "a") Add (EVar "b")
-- x * (y + 1)       -- EBinOp (EVar "x") Mul (EBinOp (EVar "y") Add (ELit (VInt 1)))
-- @
--
-- = Access Operations
--
-- * 'EArrayAccess' - Array indexing
-- * 'EFieldAccess' - Struct field access
--
-- @
-- arr[i]            -- EArrayAccess (EVar "arr") (EVar "i")
-- point.x           -- EFieldAccess (EVar "point") "x"
-- points[0].x       -- EFieldAccess (EArrayAccess (EVar "points") (ELit (VInt 0))) "x"
-- @
--
-- = Complex Expressions
--
-- * 'EIfThenElse' - Ternary conditional expression
-- * 'EDynArray' - Array literal
--
-- @
-- x > 0 ? x : -x              -- EIfThenElse (x > 0) x (-x)
-- [1, 2, 3, 4]                -- EDynArray (fromList [ELit (VInt 1), ...])
-- @
--
-- See @code-examples/04-functions/@ and @code-examples/06-arrays/@ for examples.
data Expr
  = EVar Ident                     -- ^ Variable reference
  | ELit Value                     -- ^ Literal value (carries runtime 'Value')
  | EFunCall Ident Args            -- ^ Function call: name(args)
  
  | EUnOp UnOp Expr                -- ^ Unary operation: -x, !flag
  | EBinOp Expr BinOp Expr         -- ^ Binary operation: a + b, x == y

  | EArrayAccess Expr Expr         -- ^ Array indexing: arr[i]
  | EFieldAccess Expr Ident        -- ^ Field access: struct.field

  | EIfThenElse Expr Expr Expr     -- ^ Ternary: cond ? then_expr : else_expr
  | EDynArray (Vector Expr)        -- ^ Array literal: [1, 2, 3]
  deriving (Eq)

-- | Binary operators
--
-- = Arithmetic
-- * 'Add', 'Sub', 'Mul', 'Div', 'Rem' - @+  -  *  /  %@
--
-- = Comparison
-- * 'Eq', 'NEq' - @==  !=@
-- * 'LT', 'GT', 'LEQ', 'GEQ' - @<  >  <=  >=@
--
-- = Logical
-- * 'And', 'Or' - @&&  ||@
--
-- All operators are left-associative. Precedence follows standard conventions:
-- @*@ and @/@ bind tighter than @+@ and @-@, comparisons bind looser than arithmetic,
-- logical operators bind loosest.
data BinOp
  = Add | Sub | Mul | Div | Rem     -- ^ Arithmetic: + - * / %
  | Eq  | NEq | LT | GT | LEQ | GEQ -- ^ Comparison: == != < > <= >=
  | And | Or                        -- ^ Logical: && ||
  deriving (Eq)

-- | Unary operators
--
-- * 'Neg' - Numeric negation (@-x@)
-- * 'Not' - Boolean negation (@!flag@)
data UnOp
  = Neg  -- ^ Numeric negation: -x
  | Not  -- ^ Boolean negation: !flag
  deriving (Eq)

-- ===================================================================
-- 5. MELHORIA: Instâncias 'Show' para Operadores (BinOp, UnOp)
-- ===================================================================
instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Rem = "%"
  show Eq  = "=="
  show NEq = "!="
  show AST.LT  = "<"
  show AST.GT  = ">"
  show LEQ = "<="
  show GEQ = ">="
  show And = "&&"
  show Or  = "||"

instance Show UnOp where
  show Neg = "-"
  show Not = "!"

-- =S=================================================================
-- 6. MELHORIA: Instância 'Show' bonita para Expr
-- ===================================================================

-- | Helper to format function/constructor arguments.
--
-- Joins expressions with commas inside parentheses:
--
-- @
-- showArgs [ELit (VInt 1), ELit (VInt 2)] = "(1, 2)"
-- @
showArgs :: [Expr] -> String
showArgs args = "(" ++ List.intercalate ", " (fmap show args) ++ ")"

-- | Helper to parenthesize sub-expressions based on their complexity.
--
-- Avoids ambiguity in operator precedence (e.g., @a + b * c@):
--
-- - Simple expressions (variables, literals, nullary calls) don't need parentheses
-- - Complex expressions (binary ops, ternary) get wrapped
--
-- @
-- showParenExpr (EVar "x") = "x"
-- showParenExpr (EBinOp Add (EVar "a") (EVar "b")) = "(a + b)"
-- @
showParenExpr :: Expr -> String
showParenExpr e@(EVar _)         = show e
showParenExpr e@(ELit _)         = show e
showParenExpr e@(EFunCall _ [])  = show e -- Chamada sem args
showParenExpr e@(EArrayAccess {}) = show e
showParenExpr e@(EFieldAccess {}) = show e
showParenExpr e@(EDynArray _)     = show e
-- Adiciona parênteses no resto para garantir
showParenExpr e                  = "(" ++ show e ++ ")" 

instance Show Expr where
  show (EVar ident)     = show ident -- usa o Show limpo de Ident
  show (ELit val)       = show val   -- usa o Show de Value
  show (EFunCall f args) = show f ++ showArgs args
  
  show (EUnOp op e) = show op ++ showParenExpr e
  
  show (EBinOp e1 op e2) = 
    showParenExpr e1 ++ " " ++ show op ++ " " ++ showParenExpr e2

  show (EArrayAccess arr idx) = showParenExpr arr ++ "[" ++ show idx ++ "]"
  show (EFieldAccess e fld) = showParenExpr e ++ "." ++ show fld

  show (EIfThenElse c t e) = 
    "(if " ++ show c ++ " then " ++ show t ++ " else " ++ show e ++ ")"
    
  show (EDynArray exprs) = 
    "[" ++ List.intercalate ", " (fmap show (toList exprs)) ++ "]"

-- ===================================================================
-- Stmt e Pattern agora usam 'deriving (Show)', mas como
-- seus componentes (Ident, Expr, Value, Typε) têm 'Show' bonitos,
-- a saída deles já será MUITO melhor!
-- ===================================================================

-- ===================================================================
-- A instância de Show para TypeDef (da pergunta anterior)
-- já está aqui e se beneficia automaticamente das mudanças
-- em 'Ident' e 'Typε'.
-- ===================================================================
instance Show TypeDef where

  -- Caso 1: DStruct
  show (DStruct name fields) =
    let header = "struct " ++ show name ++ " {"
        -- Formata cada campo: "  nomeCampo: TipoCampo"
        formatField (fName, fType) = "  " ++ show fName ++ ": " ++ show fType
        fieldStrings = fmap formatField (Map.toList fields)
    in
    -- Se não houver campos, imprime em uma linha: "struct Nome { }"
    if Prelude.null fieldStrings
    then header ++ " }"
    -- Se houver campos, imprime em várias linhas com indentação
    else header ++ "\n" ++ List.intercalate "\n" fieldStrings ++ "\n}"

