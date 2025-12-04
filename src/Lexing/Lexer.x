{
module Lexing.Lexer (getTokens, tokenize, Token(..), TokenType(..), alexScanTokens, prettyToken) where
import Data.Char
import System.IO
}

%wrapper "posn"

$digit = 0-9
$lower = [a-z]
$upper = [A-Z]
$alpha = [$lower $upper]

tokens :-

  $white+                                   ;
  "//".*                                    ;

  -- Palavras-chave
  "struct"                                  { \(AlexPn _ l c) _ -> Token Struct l c }
  "variant"                                 { \(AlexPn _ l c) _ -> Token Variant l c }
  "fn"                                      { \(AlexPn _ l c) _ -> Token Function l c }
  "var"                                     { \(AlexPn _ l c) _ -> Token Var l c }
  "const"                                   { \(AlexPn _ l c) _ -> Token Const l c }

  "int"                                     { \(AlexPn _ l c) _ -> Token IntType l c }
  "float"                                   { \(AlexPn _ l c) _ -> Token FloatType l c }
  "text"                                    { \(AlexPn _ l c) _ -> Token TextType l c }
  "bool"                                    { \(AlexPn _ l c) _ -> Token BoolType l c }
  "void"                                    { \(AlexPn _ l c) _ -> Token VoidType l c }

  "break"                                   { \(AlexPn _ l c) _ -> Token Stop l c }
  "again"                                   { \(AlexPn _ l c) _ -> Token Skip l c }
  "print"                                   { \(AlexPn _ l c) _ -> Token Printf l c }
  "printf"                                  { \(AlexPn _ l c) _ -> Token PrintfFmt l c }
  "abort"                                   { \(AlexPn _ l c) _ -> Token Abort l c }
  "scan"                                    { \(AlexPn _ l c) _ -> Token Scanf l c }
  "int2float"                               { \(AlexPn _ l c) _ -> Token Int2Float l c }
  "float2int"                               { \(AlexPn _ l c) _ -> Token Float2Int l c }

  "if"                                      { \(AlexPn _ l c) _ -> Token If l c }
  "else"                                    { \(AlexPn _ l c) _ -> Token Else l c }
  "while"                                   { \(AlexPn _ l c) _ -> Token While l c }
  "for"                                     { \(AlexPn _ l c) _ -> Token For l c }
  "in"                                      { \(AlexPn _ l c) _ -> Token In l c }
  "match"                                   { \(AlexPn _ l c) _ -> Token Match l c }
  "return"                                  { \(AlexPn _ l c) _ -> Token Return l c }
  "null"                                    { \(AlexPn _ l c) _ -> Token Null l c }
  "new"                                     { \(AlexPn _ l c) _ -> Token New l c }
  "drop"                                    { \(AlexPn _ l c) _ -> Token Drop l c }
  "is_null"                                 { \(AlexPn _ l c) _ -> Token IsNull l c }
  "import"                                  { \(AlexPn _ l c) _ -> Token Import l c }

  -- Símbolos (multi-char operators MUST come before single-char to avoid ambiguity)
  -- Array operators
  "=<<"                                     { \(AlexPn _ l c) _ -> Token ArrayPush l c }
  ">>="                                     { \(AlexPn _ l c) _ -> Token ArrayCons l c }
  "<>"                                      { \(AlexPn _ l c) _ -> Token ArrayConcat l c }
  "!>>"                                     { \(AlexPn _ l c) _ -> Token ArrayUncons l c }
  "!<<"                                     { \(AlexPn _ l c) _ -> Token ArrayDrop l c }
  
  -- Comparison and logical operators (multi-char)
  "=="                                      { \(AlexPn _ l c) _ -> Token Equals l c }
  "!="                                      { \(AlexPn _ l c) _ -> Token NotEquals l c }
  ">="                                      { \(AlexPn _ l c) _ -> Token GreaterEquals l c }
  "<="                                      { \(AlexPn _ l c) _ -> Token LesserEquals l c }
  "=>"                                      { \(AlexPn _ l c) _ -> Token FatArrow l c }
  "::"                                      { \(AlexPn _ l c) _ -> Token DoubleColon l c }
  "||"                                      { \(AlexPn _ l c) _ -> Token Or l c }
  "&&"                                      { \(AlexPn _ l c) _ -> Token And l c }

  -- Brackets and braces
  "{"                                       { \(AlexPn _ l c) _ -> Token BeginBrace l c }
  "}"                                       { \(AlexPn _ l c) _ -> Token EndBrace l c }
  "["                                       { \(AlexPn _ l c) _ -> Token BeginBracket l c }
  "]"                                       { \(AlexPn _ l c) _ -> Token EndBracket l c }
  "("                                       { \(AlexPn _ l c) _ -> Token BeginParentheses l c }
  ")"                                       { \(AlexPn _ l c) _ -> Token EndParentheses l c }
  
  -- Single-char symbols
  ";"                                       { \(AlexPn _ l c) _ -> Token SemiColon l c }
  "."                                       { \(AlexPn _ l c) _ -> Token Dot l c }
  ","                                       { \(AlexPn _ l c) _ -> Token Comma l c }
  ":"                                       { \(AlexPn _ l c) _ -> Token Colon l c }
  "?"                                       { \(AlexPn _ l c) _ -> Token QuestionMark l c }
  ">"                                       { \(AlexPn _ l c) _ -> Token Greater l c }
  "<"                                       { \(AlexPn _ l c) _ -> Token Lesser l c }
  "="                                       { \(AlexPn _ l c) _ -> Token Assign l c }
  "_"                                       { \(AlexPn _ l c) _ -> Token Underscore l c }
  "!"                                       { \(AlexPn _ l c) _ -> Token Not l c }
  "+"                                       { \(AlexPn _ l c) _ -> Token Add l c }
  "-"                                       { \(AlexPn _ l c) _ -> Token Sub l c }
  "*"                                       { \(AlexPn _ l c) _ -> Token Mul l c }
  "/"                                       { \(AlexPn _ l c) _ -> Token Div l c }
  "%"                                       { \(AlexPn _ l c) _ -> Token Rem l c }
  "&"                                       { \(AlexPn _ l c) _ -> Token Ampersand l c }
  "@"                                       { \(AlexPn _ l c) _ -> Token UnitLit l c }

  -- Literais
  $digit+\.$digit+                          { \(AlexPn _ l c) s -> Token (FloatLit (read s)) l c }
  $digit+                                   { \(AlexPn _ l c) s -> Token (IntLit (read s)) l c }
  "true"                                    { \(AlexPn _ l c) _ -> Token (BoolLit True) l c }
  "false"                                   { \(AlexPn _ l c) _ -> Token (BoolLit False) l c }
  \" ([^\"\\] | \\ [nrt\"\\])* \"           { \(AlexPn _ l c) s -> Token (TextLit (unescape (init (tail s)))) l c }

  -- Identificadores
  $alpha [$alpha $digit _]*                 { \(AlexPn _ l c) s ->
      if isUpper (head s)
      then Token (TypeId s) l c
      else Token (VarId s) l c
  }


{
-- ========================================
-- Definição dos tipos de tokens
-- ========================================
data Token = Token
  { tokenType :: TokenType --todo
  , tokenLine :: Int
  , tokenCol  :: Int
  } deriving (Eq, Show, Ord)

data TokenType =
    -- Palavras-chave
    Struct | Variant | Function | Var | Const
  | Stop | Skip | Printf | PrintfFmt | Abort | Scanf
  | Int2Float | Float2Int  -- Conversion builtins
  | If | Else | Match | While | For | In | Return | Null
  | New | Drop | IsNull  -- Heap management and null checking
  | Import  -- Module system (future)

    -- Tipos primitivos
  | IntType | FloatType | TextType | BoolType | VoidType

    -- Símbolos
  | BeginBrace | EndBrace
  | BeginBracket | EndBracket
  | BeginParentheses | EndParentheses
  | SemiColon | Dot | Comma | DoubleColon | Colon | QuestionMark
  | Equals | NotEquals | GreaterEquals | LesserEquals | FatArrow
  | Assign | Not | Greater | Lesser | Underscore
  | Or | And | Add | Sub | Mul | Div | Rem | Ampersand

    -- Array operators (future implementation)
  | ArrayPush    -- =<< - push element to end (arr =<< elem)
  | ArrayCons    -- >>= - cons element to beginning (elem >>= arr)
  | ArrayConcat  -- <> - concatenate arrays
  | ArrayUncons  -- !>> - remove first element (!>> arr)
  | ArrayDrop    -- !<< - drop first element (arr !<<)

    -- Literais
  | IntLit Integer
  | FloatLit Double
  | TextLit String
  | BoolLit Bool
  | UnitLit  -- @ - Unit/void literal

    -- Identificadores
  | TypeId String
  | VarId String

  deriving (Eq, Show, Ord)

-- ========================================
-- Função para desescapar strings
-- ========================================
unescape :: String -> String
unescape ('\\':'n':xs) = '\n' : unescape xs
unescape ('\\':'r':xs) = '\r' : unescape xs
unescape ('\\':'t':xs) = '\t' : unescape xs
unescape ('\\':'\\':xs) = '\\' : unescape xs
unescape ('\\':'"':xs) = '"' : unescape xs
unescape (x:xs) = x : unescape xs
unescape [] = []

-- ========================================
-- Função para ler arquivo e gerar tokens
-- ========================================
getTokens :: FilePath -> IO [Token]
getTokens path = do
  content <- readFile path
  return (alexScanTokens content)

-- | Tokenize a string directly (useful for LSP)
tokenize :: FilePath -> String -> [Token]
tokenize _filepath content = alexScanTokens content

-- | Pretty print a token for debugging/error messages
prettyToken :: Token -> String
prettyToken (Token ttype line col) = 
  show ttype ++ " at line " ++ show line ++ ", col " ++ show col
}
