{-|
Module      : Parser.Stmt
Description : Statement parsing utilities for Gloom
Copyright   : (c) JJoaoll, 2025
License     : MIT
Maintainer  : jjoaoll@example.com

This module provides parsing utilities for Gloom statements that are
used across different parts of the parser. It contains low-level statement
parsers that handle control flow structures like blocks, returns, and
conditionals.

These utilities are building blocks used by "Parser.Fun" to construct
the complete statement grammar.
-}
module Parser.Stmt
  ( -- * Type Aliases
    CheckParser

    -- * Return Analysis
  , checkGuaranteedReturn

    -- * Block Checking
  , checkBlock
  , checkStmt

    -- * Statement Checking
  , checkSReturn
  , checkIfElse
  , checkWhile
  , checkFor
  , skipOtherStmt
  ) where

--global 

import Control.Monad (void)
import Lexing.Lexer (Token)

import qualified Lexing.Tokens.Keywords as Token
import qualified Lexing.Tokens.Symbols as Token
import Text.Parsec (anyToken, lookAhead, manyTill, ParsecT, ParseError, runParserT, choice, try, optionMaybe)

-- | Type alias for parsers that check return guarantees.
-- Used in static analysis to verify all code paths return.




-- i jsut wanna use this file: AI:
--
--import Text.Parsec hiding (token, tokens, label, optional, try)

-- | Nosso novo tipo de parser. Não usa 'RTState' e retorna 'a'.
type CheckParser a = ParsecT [Token] () IO a

-- | Ponto de entrada. Recebe o corpo da função (incluindo as chaves)
-- | e retorna True se o retorno for garantido.
checkGuaranteedReturn :: [Token] -> IO (Either ParseError Bool)
checkGuaranteedReturn toks =
    -- Executa o checkBlock no stream de tokens
    runParserT (do
    _ <- Token.openBrace

    -- Parseia todos os statements até o 'closeBrace'
    -- 'guarantees' será uma lista de Bools
    guarantees <- manyTill checkStmt (lookAhead Token.closeBrace)

    _ <- Token.closeBrace

    -- 'any id guarantees' é equivalente a 'or (guarantees)'
    -- Se *qualquer* statement garantiu o retorno, o bloco garante.
    pure $ or guarantees) () "functionBody" toks

-- | Analisa um bloco: { stmt1; stmt2; ... }
-- | Um bloco garante retorno se *qualquer* statement dentro dele garantir.
-- | (Ex: if/else completo, ou um return direto)
checkBlock :: CheckParser Bool
checkBlock = do
    _ <- Token.openBrace

    -- Parseia todos os statements até o 'closeBrace'
    -- 'guarantees' será uma lista de Bools
    guarantees <- manyTill checkStmt (lookAhead Token.closeBrace)

    _ <- Token.closeBrace

    -- 'any id guarantees' é equivalente a 'or (guarantees)'
    -- Se *qualquer* statement garantiu o retorno, o bloco garante.
    pure $ or guarantees

-- | Analisa um único statement e retorna True se *ele* garante o retorno.
checkStmt :: CheckParser Bool
checkStmt = choice
    [ try checkSReturn
    , try checkIfElse
    , try checkWhile     -- Precisa vir antes de skipOtherStmt
    , try checkFor       -- Precisa vir antes de skipOtherStmt
    , skipOtherStmt    -- O fallback para qualquer outra coisa
    ]

-- | Analisa um 'return'. Garante retorno.
checkSReturn :: CheckParser Bool
checkSReturn = do
    _ <- Token.return
    -- Consome a expressão (qualquer token até o ';')
    _ <- manyTill anyToken (lookAhead Token.semiColon)
    _ <- Token.semiColon
    pure True -- O 'return' sempre garante o retorno.

-- | Analisa um 'if' (e 'else' opcional).
checkIfElse :: CheckParser Bool
checkIfElse = do
    _ <- Token.iff
    -- Consome a expressão da condição
    _ <- manyTill anyToken (lookAhead Token.openBrace)

    -- Analisa o bloco 'then'.
    thenGuarantees <- checkBlock

    -- Procura por um 'else'
    mElse <- optionMaybe (lookAhead Token.elsε)

    case mElse of
        -- Caso 1: 'if' sem 'else'. Nunca garante retorno.
        Nothing -> pure False

        -- Caso 2: 'if' com 'else'.
        Just _ -> do
            _ <- Token.elsε -- Consome o token 'else'

            -- O 'else' pode ser um 'else if' ou um 'else { ... }'
            -- Verificamos se o próximo token é 'if'
            isElseIf <- optionMaybe (lookAhead Token.iff)

            elseGuarantees <- case isElseIf of
                -- 'else if ...' : recursivamente chama o parser de if
                Just _  -> checkIfElse
                -- 'else { ... }' : parseia o bloco 'else'
                Nothing -> checkBlock

            -- Garante retorno *apenas se* AMBOS os ramos garantirem.
            pure (thenGuarantees && elseGuarantees)

-- | Analisa um 'while'. Nunca garante retorno.
checkWhile :: CheckParser Bool
checkWhile = do
    _ <- Token.while
    -- Consome a condição
    _ <- manyTill anyToken (lookAhead Token.openBrace)
    -- Consome o bloco (não nos importa o que há dentro)
    void checkBlock
    pure False -- 'while' nunca garante, pois pode não rodar.

-- | Analisa um 'for'. Nunca garante retorno.
checkFor :: CheckParser Bool
checkFor = do
    _ <- Token.for
    -- Consome o header do 'for' (ex: 'i in list')
    _ <- manyTill anyToken (lookAhead Token.openBrace)
    -- Consome o bloco
    void checkBlock
    pure False -- 'for' nunca garante, pois pode não rodar.

-- | Consome qualquer outro statement que não seja de controle de fluxo.
-- | (atribuição, declaração, print, etc.).
-- | Assumimos que todos terminam em ';'.
skipOtherStmt :: CheckParser Bool
skipOtherStmt = do
    -- Consome qualquer token que não seja um 'return', 'if', 'while', 'for'
    -- até encontrar um ';'
    _ <- manyTill anyToken (lookAhead Token.semiColon)
    _ <- Token.semiColon
    pure False -- Nenhum desses statements garante retorno.

-- | Um parser genérico para consumir qualquer token.
-- | Usado por 'manyTill'.
-- anyToken :: CheckParser Token
-- anyToken = satisfy (const True)
