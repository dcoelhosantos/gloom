{-|
Module      : Lexing.Tokens.Keywords
Description : Keyword token parsers for the Gloom lexer
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module provides parsers for all Gloom language keywords.
Each function matches a specific keyword token from the token stream.

= Type System Keywords
* 'struct' - Struct type definition
* 'variant' - Variant type definition (sum types)

= Declaration Keywords
* 'fn' - Function definition
* 'var' - Variable declaration
* 'const' - Constant declaration

= Control Flow Keywords
* 'iff' - If statement (@if@ conflicts with Prelude)
* 'elsε' - Else clause (ε for Greek flair)
* 'while' - While loop
* 'for' - For-each loop
* 'inn' - In keyword for for-each (@in@ conflicts with Prelude)
* 'break' - Break from loop
* 'again' - Continue loop (originally @continue@)
* 'return' - Return from function

= I/O Keywords
* 'print' - Print to stdout
* 'printf' - Formatted print to stdout (C-style)
* 'scan' - Read from stdin

= Type Conversion Keywords
* 'int2float' - Convert integer to float
* 'float2int' - Convert float to integer

= Memory Keywords
* 'new' - Heap allocation
* 'drop' - Heap deallocation

= Pattern Matching (Experimental)
* 'mαtch' - Pattern match statement (α for Greek flair)
-}
module Lexing.Tokens.Keywords
  ( -- * Type System Keywords
    struct
  , variant

    -- * Declaration Keywords
  , fn
  , var
  , Lexing.Tokens.Keywords.const

    -- * Control Flow Keywords
  , iff
  , elsε
  , while
  , for
  , inn
  , Lexing.Tokens.Keywords.break
  , again
  , Lexing.Tokens.Keywords.return

    -- * I/O Keywords
  , Lexing.Tokens.Keywords.print
  , printf
  , abort
  , scan

    -- * Type Conversion Keywords
  , int2float
  , float2int

    -- * Memory Keywords
  , new
  , Lexing.Tokens.Keywords.drop
  , isNull

    -- * Module Keywords
  , import_

    -- * Pattern Matching Keywords
  , mαtch
  ) where

-- global

-- local
import Lexing.Tokens.General
import Lexing.Lexer

-- | Match @struct@ keyword for struct definitions
struct :: Parser u Token
struct = match Struct

-- | Match @variant@ keyword for variant definitions
variant :: Parser u Token
variant = match Variant

-- | Match @fn@ keyword for function definitions
fn :: Parser u Token
fn = match Function

-- | Match @var@ keyword for variable declarations
var :: Parser u Token
var = match Var

-- | Match @const@ keyword for constant declarations
const :: Parser u Token
const = match Lexing.Lexer.Const 

-- | Match @break@ keyword for breaking out of loops
break :: Parser u Token
break = match Stop

-- | Match @again@ keyword for continuing loops (like @continue@ in C)
again :: Parser u Token
again = match Skip

-- | Match @print@ keyword for output
print :: Parser u Token
print = match Printf

-- | Match @printf@ keyword for formatted output (C-style)
printf :: Parser u Token
printf = match PrintfFmt

-- | Match @abort@ keyword for panic/error abort
abort :: Parser u Token
abort = match Abort

-- | Match @scan@ keyword for input
scan :: Parser u Token
scan = match Scanf

-- | Match @int2float@ keyword for integer to float conversion
int2float :: Parser u Token
int2float = match Int2Float

-- | Match @float2int@ keyword for float to integer conversion
float2int :: Parser u Token
float2int = match Float2Int

-- | Match @if@ keyword (named @iff@ to avoid Prelude conflict)
iff :: Parser u Token
iff = match If 

-- | Match @else@ keyword (Greek epsilon ε for style)
elsε :: Parser u Token
elsε = match Else

-- | Match @match@ keyword for pattern matching (Greek alpha α for style)
mαtch :: Parser u Token
mαtch = match Match 

-- | Match @while@ keyword for while loops
while :: Parser u Token
while = match While

-- | Match @for@ keyword for for-each loops
for :: Parser u Token
for = match For

-- | Match @in@ keyword for for-each (named @inn@ to avoid Prelude conflict)
inn :: Parser u Token
inn = match In

-- | Match @return@ keyword for function returns
return :: Parser u Token
return = match Return

-- | Match @new@ keyword for heap allocation
new :: Parser u Token
new = match New

-- | Match @drop@ keyword for heap deallocation
drop :: Parser u Token
drop = match Drop

-- | Match @is_null@ keyword for null pointer check
isNull :: Parser u Token
isNull = match IsNull

-- | Match @import@ keyword for module imports
import_ :: Parser u Token
import_ = match Import
