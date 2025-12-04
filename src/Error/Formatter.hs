{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Error.Formatter
Description : Professional error formatting for Gloom compiler
Copyright   : (c) JJoaoll, 2025
License     : MIT

This module provides Rust/Gleam-style error formatting with:
- Source code snippets with line numbers
- Color-coded error messages
- Helpful suggestions and context
- Precise error location highlighting
-}
module Error.Formatter
  ( -- * Error Formatting
    formatError
  , formatTypeError
  , formatRuntimeError
  , formatInternalError
  , formatCompileError
  , formatConditionError
  , formatConstError
  , formatDivByZeroError
  , formatNullDerefError
  , formatArrayTypeMismatchError
  , formatNotFoundError
  , formatExpectedTypeError
  , formatArgMismatchError
  , formatCircularImportError
  , formatFileNotFoundError
    
    -- * Components
  , errorHeader
  , sourceSnippet
  , errorPointer
  , helpMessage
  , noteMessage
  
    -- * Colors
  , red
  , yellow
  , blue
  , cyan
  , bold
  , reset
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | ANSI color codes
red, yellow, blue, cyan, bold, reset :: Text
red    = "\x1b[31m"
yellow = "\x1b[33m"
blue   = "\x1b[34m"
cyan   = "\x1b[36m"
bold   = "\x1b[1m"
reset  = "\x1b[0m"

-- | Format error header (e.g., "error[E0001]: Type mismatch")
errorHeader :: Text -> Text -> Text
errorHeader code msg = 
  bold <> red <> "error" <> reset <> bold <> "[" <> code <> "]: " <> reset <> msg

-- | Format a source code snippet with line numbers and error pointer
sourceSnippet :: FilePath -> Int -> Int -> Text -> Text
sourceSnippet filepath line col sourceText =
  let lineStr = T.pack $ show line
      padding = T.replicate (T.length lineStr) " "
      arrow = blue <> "-->" <> reset
      location = bold <> T.pack filepath <> ":" <> lineStr <> ":" <> T.pack (show col) <> reset
  in T.unlines
       [ ""
       , padding <> " " <> arrow <> " " <> location
       , padding <> " " <> blue <> "|" <> reset
       , lineStr <> " " <> blue <> "|" <> reset <> " " <> sourceText
       , padding <> " " <> blue <> "|" <> reset <> " " <> errorPointer col (T.length sourceText)
       ]

-- | Create error pointer (e.g., "    ^^^^")
errorPointer :: Int -> Int -> Text
errorPointer col width =
  let spaces = T.replicate (col - 1) " "
      carets = red <> "^" <> T.replicate (max 0 (min 10 (width - col))) "^" <> reset
  in spaces <> carets

-- | Format a help message
helpMessage :: Text -> Text
helpMessage msg = 
  blue <> "help: " <> reset <> msg

-- | Format a note message  
noteMessage :: Text -> Text
noteMessage msg =
  cyan <> "note: " <> reset <> msg

-- | Format a type error with full context
formatTypeError :: FilePath -> Int -> Int -> Text -> Text -> Text -> Text -> Text
formatTypeError filepath line col sourceText expected got suggestion =
  let header = errorHeader "E0308" "type mismatch"
      snippet = sourceSnippet filepath line col sourceText
      expectedMsg = "expected type: " <> bold <> expected <> reset
      gotMsg = "   found type: " <> bold <> got <> reset
      help = if T.null suggestion 
             then "" 
             else "\n" <> helpMessage suggestion
  in T.unlines
       [ header
       , snippet
       , expectedMsg
       , gotMsg
       , help
       ]

-- | Format a runtime error
formatRuntimeError :: FilePath -> Int -> Int -> Text -> Text -> Text -> Text
formatRuntimeError filepath line col sourceText errorMsg details =
  let header = errorHeader "E1000" errorMsg
      snippet = sourceSnippet filepath line col sourceText
      note = if T.null details
             then ""
             else "\n" <> noteMessage details
  in T.unlines
       [ header
       , snippet
       , note
       ]

-- | Format an internal compiler error (should never happen)
formatInternalError :: Text -> Text -> Text
formatInternalError location msg =
  T.unlines
    [ bold <> red <> "internal compiler error: " <> reset <> msg
    , ""
    , cyan <> "note: " <> reset <> "the compiler unexpectedly panicked. This is a bug."
    , cyan <> "note: " <> reset <> "location: " <> location
    , ""
    , "Please report this bug at: https://github.com/JJoaoll/bird/issues"
    ]

-- | Format a general compilation error
formatCompileError :: FilePath -> Int -> Int -> Text -> Text -> Text -> Text
formatCompileError filepath line col sourceText errorType msg =
  let header = errorHeader "E0001" errorType
      snippet = sourceSnippet filepath line col sourceText
  in T.unlines
       [ header
       , snippet
       , msg
       ]

-- | Generic error formatter (main entry point)
formatError :: FilePath -> Int -> Int -> Text -> Text -> Maybe Text -> Text
formatError filepath line col sourceText errorMsg maybeHelp =
  let header = errorHeader "E0000" errorMsg
      snippet = sourceSnippet filepath line col sourceText
      help = case maybeHelp of
               Just h  -> "\n" <> helpMessage h
               Nothing -> ""
  in T.unlines
       [ header
       , snippet
       , help
       ]

-- | Format a condition error (if/while/etc must be boolean)
formatConditionError :: FilePath -> Int -> Int -> Text -> Text -> Text -> Text
formatConditionError filepath line col sourceText condType actualType =
  let header = errorHeader "E0308" $ "condition in '" <> condType <> "' must be boolean"
      snippet = sourceSnippet filepath line col sourceText
      expectedMsg = "expected type: " <> bold <> "bool" <> reset
      gotMsg = "   found type: " <> bold <> actualType <> reset
      help = helpMessage $ "change the condition to return a boolean value"
  in T.unlines
       [ header
       , snippet
       , expectedMsg
       , gotMsg
       , help
       ]

-- | Format a const modification error
formatConstError :: FilePath -> Int -> Int -> Text -> Text -> Text
formatConstError filepath line col sourceText targetName =
  let header = errorHeader "E0594" "cannot modify immutable variable"
      snippet = sourceSnippet filepath line col sourceText
      note = noteMessage $ "'" <> targetName <> "' is declared as const"
      help = helpMessage $ "remove 'const' from the variable declaration to make it mutable"
  in T.unlines
       [ header
       , snippet
       , note
       , help
       ]

-- | Format a division by zero error
formatDivByZeroError :: FilePath -> Int -> Int -> Text -> Text -> Text
formatDivByZeroError filepath line col sourceText opType =
  let header = errorHeader "E1001" $ opType <> " division by zero"
      snippet = sourceSnippet filepath line col sourceText
      note = noteMessage $ "division by zero causes a runtime panic"
      help = helpMessage "add a check before dividing: if (divisor != 0) { ... }"
  in T.unlines
       [ header
       , snippet
       , note
       , help
       ]

-- | Format a null pointer dereference error
formatNullDerefError :: FilePath -> Int -> Int -> Text -> Text
formatNullDerefError filepath line col sourceText =
  let header = errorHeader "E1002" "null pointer dereference"
      snippet = sourceSnippet filepath line col sourceText
      note = noteMessage "attempting to dereference a null pointer causes a runtime panic"
      help = helpMessage "check if the pointer is null before dereferencing: if (ptr != null) { ... }"
  in T.unlines
       [ header
       , snippet
       , note
       , help
       ]

-- | Format an array type mismatch error
formatArrayTypeMismatchError :: FilePath -> Int -> Int -> Text -> Text -> Text -> Text
formatArrayTypeMismatchError filepath line col sourceText expectedType gotType =
  let header = errorHeader "E0308" "array elements must have the same type"
      snippet = sourceSnippet filepath line col sourceText
      expectedMsg = "expected type: " <> bold <> expectedType <> reset
      gotMsg = "   found type: " <> bold <> gotType <> reset
      help = helpMessage "ensure all array elements are of the same type"
  in T.unlines
       [ header
       , snippet
       , expectedMsg
       , gotMsg
       , help
       ]

-- | Format a not found error (variable, function, constructor, etc)
formatNotFoundError :: FilePath -> Int -> Int -> Text -> Text -> Text -> Text
formatNotFoundError filepath line col sourceText itemType itemName =
  let header = errorHeader "E0425" $ itemType <> " not found"
      snippet = sourceSnippet filepath line col sourceText
      note = noteMessage $ "cannot find " <> itemType <> " '" <> itemName <> "' in this scope"
      help = helpMessage $ "check the spelling or declare the " <> itemType <> " before using it"
  in T.unlines
       [ header
       , snippet
       , note
       , help
       ]

-- | Format a type error for operations (expecting struct, array, etc)
formatExpectedTypeError :: FilePath -> Int -> Int -> Text -> Text -> Text -> Text
formatExpectedTypeError filepath line col sourceText operation expectedType =
  let header = errorHeader "E0308" $ "type error in " <> operation
      snippet = sourceSnippet filepath line col sourceText
      note = noteMessage $ "this operation expects " <> bold <> expectedType <> reset
      help = helpMessage $ "ensure the value is of type " <> expectedType
  in T.unlines
       [ header
       , snippet
       , note
       , help
       ]

-- | Format an argument mismatch error
formatArgMismatchError :: FilePath -> Int -> Int -> Text -> Text -> Int -> Int -> Text
formatArgMismatchError filepath line col sourceText funcName expected got =
  let header = errorHeader "E0061" "argument mismatch"
      snippet = sourceSnippet filepath line col sourceText
      expectedMsg = "expected: " <> bold <> T.pack (show expected) <> " arguments" <> reset
      gotMsg = "   found: " <> bold <> T.pack (show got) <> " arguments" <> reset
      note = noteMessage $ "function '" <> funcName <> "' expects " <> T.pack (show expected) <> " arguments"
  in T.unlines
       [ header
       , snippet
       , expectedMsg
       , gotMsg
       , note
       ]

-- | Format a circular import error with import cycle trace
formatCircularImportError :: FilePath -> Int -> Int -> Text -> FilePath -> [FilePath] -> Text
formatCircularImportError filepath line col sourceText targetFile importCycle =
  let header = errorHeader "E0425" "circular dependency detected"
      snippet = sourceSnippet filepath line col sourceText
      cycleLines = T.unlines $ map formatCycleLine (zip [0..] importCycle)
      note = noteMessage "import cycle detected:"
      help1 = helpMessage "reorganize your modules to break the circular dependency"
      help2 = helpMessage "consider extracting shared code into a third module"
  in T.unlines
       [ header
       , snippet
       , note
       , cycleLines
       , help1
       , help2
       ]
  where
    formatCycleLine :: (Int, FilePath) -> Text
    formatCycleLine (depth, file) =
      let indent = T.replicate (depth * 2) " "
          arrow = if depth > 0 then "└─> " else "    "
          marker = if file == targetFile then bold <> cyan <> " (cycle!)" <> reset else ""
      in indent <> arrow <> T.pack file <> marker

-- | Format a file not found error
formatFileNotFoundError :: FilePath -> Int -> Int -> Text -> FilePath -> Text
formatFileNotFoundError filepath line col sourceText missingFile =
  let header = errorHeader "E0583" "file not found for module"
      snippet = sourceSnippet filepath line col sourceText
      note = noteMessage $ "no file found at: " <> bold <> T.pack missingFile <> reset
      help1 = helpMessage "check the file path and ensure the file exists"
      help2 = helpMessage "import paths are relative to the current file"
  in T.unlines
       [ header
       , snippet
       , note
       , help1
       , help2
       ]
