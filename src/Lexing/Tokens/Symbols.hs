{-|
Module      : Lexing.Tokens.Symbols
Description : Symbol and operator token parsers for Gloom
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module provides parsers for all Gloom punctuation and operator symbols.

= Delimiters
* 'openParens', 'closeParens' - @(@, @)@
* 'openBrace', 'closeBrace' - @{@, @}@
* 'openBracket', 'closeBracket' - @[@, @]@

= Punctuation
* 'semiColon' - @;@
* 'dot' - @.@
* 'comma' - @,@
* 'colon' - @:@
* 'doubleColon' - @::@
* 'questionMark' - @?@ (ternary operator)

= Comparison Operators
* 'equals' - @==@
* 'notEquals' - @!=@
* 'lesser' - @<@
* 'greater' - @>@
* 'lesserEquals' - @<=@
* 'greaterEquals' - @>=@

= Logical Operators
* 'and' - @&&@
* 'or' - @||@
* 'not' - @!@

= Arithmetic Operators
* 'add' - @+@
* 'sub' - @-@
* 'mul' - @*@
* 'div' - @/@
* 'rem' - @%@ (remainder)

= Assignment & References
* 'assign' - @=@
* 'ampersand' - @&@ (reference operator)

= Pattern Matching (Experimental)
* 'fatArrow' - @=>@
* 'underscore' - @_@ (wildcard pattern)
-}
module Lexing.Tokens.Symbols
  ( -- * Delimiters
    openParens
  , closeParens
  , openBrace
  , closeBrace
  , openBracket
  , closeBracket

    -- * Punctuation
  , semiColon
  , dot
  , comma
  , colon
  , doubleColon
  , questionMark

    -- * Comparison Operators
  , equals
  , notEquals
  , lesser
  , greater
  , lesserEquals
  , greaterEquals

    -- * Logical Operators
  , Lexing.Tokens.Symbols.and
  , Lexing.Tokens.Symbols.or
  , Lexing.Tokens.Symbols.not

    -- * Arithmetic Operators
  , add
  , sub
  , mul
  , Lexing.Tokens.Symbols.div
  , Lexing.Tokens.Symbols.rem

    -- * Assignment & References
  , assign
  , ampersand

    -- * Pattern Matching
  , fatArrow
  , underscore

    -- * Array Operations
  , arrayConcat
  , arrayCons
  , arrayPush
  , arrayUncons
  , arrayDrop
  ) where

-- global

-- local
import Lexing.Tokens.General
import Lexing.Lexer

-- | Match @(@ - open parenthesis
openParens :: Parser u Token
openParens = match BeginParentheses

-- | Match @)@ - close parenthesis
closeParens :: Parser u Token
closeParens = match EndParentheses

-- | Match @{@ - open brace
openBrace :: Parser u Token
openBrace = match BeginBrace

-- | Match @}@ - close brace
closeBrace :: Parser u Token
closeBrace = match EndBrace

-- | Match @[@ - open bracket
openBracket :: Parser u Token
openBracket = match BeginBracket

-- | Match @]@ - close bracket
closeBracket :: Parser u Token
closeBracket = match EndBracket

-- | Match @;@ - semicolon (statement terminator)
semiColon :: Parser u Token
semiColon = match SemiColon

-- | Match @.@ - dot (field access)
dot :: Parser u Token
dot = match Dot

-- | Match @,@ - comma (separator)
comma :: Parser u Token
comma = match Comma

-- | Match @::@ - double colon (variant constructor)
doubleColon :: Parser u Token
doubleColon = match DoubleColon

-- | Match @:@ - colon (type annotation)
colon :: Parser u Token
colon = match Colon

-- | Match @?@ - question mark (ternary operator)
questionMark :: Parser u Token
questionMark = match QuestionMark

-- | Match @==@ - equality comparison
equals :: Parser u Token
equals = match Equals

-- | Match @!=@ - inequality comparison
notEquals :: Parser u Token
notEquals = match NotEquals

-- | Match @=>@ - fat arrow (pattern matching)
fatArrow :: Parser u Token
fatArrow = match FatArrow

-- | Match @_@ - underscore (wildcard pattern)
underscore :: Parser u Token
underscore = match Underscore

-- | Match @<=@ - less than or equal
lesserEquals :: Parser u Token
lesserEquals = match LesserEquals

-- | Match @>=@ - greater than or equal
greaterEquals :: Parser u Token
greaterEquals = match GreaterEquals

-- | Match @=@ - assignment operator
assign :: Parser u Token
assign = match Assign 

-- | Match @!@ - logical NOT
not :: Parser u Token
not = match Lexing.Lexer.Not

-- | Match @<@ - less than
lesser :: Parser u Token
lesser = match Lesser 

-- | Match @>@ - greater than
greater :: Parser u Token
greater = match Greater

-- | Match @||@ - logical OR
or :: Parser u Token
or = match Lexing.Lexer.Or 

-- | Match @&&@ - logical AND
and :: Parser u Token
and = match Lexing.Lexer.And

-- | Match @+@ - addition
add :: Parser u Token
add = match Lexing.Lexer.Add

-- | Match @*@ - multiplication
mul :: Parser u Token
mul = match Lexing.Lexer.Mul

-- | Match @-@ - subtraction
sub :: Parser u Token
sub = match Lexing.Lexer.Sub

-- | Match @/@ - division
div :: Parser u Token
div = match Lexing.Lexer.Div

-- | Match @%@ - remainder (modulo)
rem :: Parser u Token
rem = match Lexing.Lexer.Rem

-- | Match @&@ - reference operator (take address)
ampersand :: Parser u Token
ampersand = match Ampersand 

-- | Match @<>@ - array concatenation (expression, returns new array)
arrayConcat :: Parser u Token
arrayConcat = match ArrayConcat

-- | Match @>:@ - array cons (prepend element - statement, modifies in-place)
arrayCons :: Parser u Token
arrayCons = match ArrayCons

-- | Match @:<@ - array push (append element - statement, modifies in-place)
arrayPush :: Parser u Token
arrayPush = match ArrayPush

-- | Match @<:@ - array uncons (remove first - statement, modifies in-place)
arrayUncons :: Parser u Token
arrayUncons = match ArrayUncons

-- | Match @:>@ - array drop (drop first - statement, modifies in-place)
arrayDrop :: Parser u Token
arrayDrop = match ArrayDrop
