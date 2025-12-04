{-|
Module      : Main
Description : Entry point for the Gloom compiler and interpreter
Copyright   : (c) João, 2024-2025
License     : MIT
Stability   : experimental

This is the main entry point for the Gloom language implementation.
It provides a professional command-line interface and orchestrates 
the two-phase execution model:

1. __Analysing Phase__: Static type checking and validation
2. __Running Phase__: Actual program execution

= Usage

@
\$ gloom myfile.gloom              -- Execute file
\$ gloom --check myfile.gloom      -- Static analysis only
\$ gloom --verbose myfile.gloom    -- Verbose output
\$ gloom --quiet myfile.gloom      -- Minimal output
\$ gloom --version                 -- Show version
\$ gloom --help                    -- Show help
@

= Two-Phase Execution

Gloom uses a unique two-phase model inspired by Rust's borrow checker,
but without the pain:

* __Phase 1 (Analysing)__: Parse the program, build symbol tables, check types,
  validate const initialization, etc. Uses placeholder values.

* __Phase 2 (Running)__: Re-parse with actual runtime state preserved from
  Phase 1. Execute statements and evaluate expressions.

This separation allows compile-time guarantees while maintaining
the simplicity of an interpreted language.

= Exit Codes

The interpreter uses standard exit codes:

* @0@: Success
* @1@: Static analysis failure (parse errors, type errors, etc.)
* @2@: Runtime execution failure
* @3@: Entry point not found (no 'main' function)

= Example Output

Normal mode:
@
\$ gloom hello-world.gloom
Running: hello-world.gloom
"Hello from Gloom!"
@

Verbose mode:
@
\$ gloom --verbose hello-world.gloom
Running: hello-world.gloom
Phase 1: Static Analysis (Analysing)
Static analysis passed
Phase 2: Execution (Running)
Entry point found: main()
"Hello from Gloom!"
Program executed successfully
@

Check mode:
@
\$ gloom --check hello-world.gloom
Running: hello-world.gloom
Check completed successfully
@
-}
module Main (main) where

import System.Exit (exitSuccess, exitWith, ExitCode(..))
import Control.Monad (when, unless)
import Text.Parsec (runParserT, getState)
import qualified System.IO as IO
import Options.Applicative
import qualified System.Process
import Data.List (isInfixOf)

-- Local imports
import Parser.TopLevel (program)
import Lexing.Lexer (getTokens)
import RTState (freshState, rtFlag, Flag(..), checkUnresolvedDebits, rtFuns)
import qualified Data.Map as Map

-- ANSI color codes for CLI errors
y, r, b, c, bold, reset :: String
y = "\x1b[33m"  -- yellow
r = "\x1b[31m"  -- red
b = "\x1b[34m"  -- blue
c = "\x1b[36m"  -- cyan
bold = "\x1b[1m"
reset = "\x1b[0m"

-- | Opens the Haddock documentation in the default browser/application
-- First checks if documentation exists, if not generates it with cabal haddock
openDocumentation :: IO ()
openDocumentation = do
  putStrLn "Checking for Haddock documentation..."
  
  -- Try to find existing documentation using shell command
  (_, stdout, _) <- System.Process.readProcessWithExitCode "find" 
    ["dist-newstyle", "-name", "index.html", "-path", "*/bird/*"] ""
  
  let existingDocs = lines stdout
  
  docPath <- if not (null existingDocs) && not (null (existingDocs !! 0))
    then do
      putStrLn "Found existing documentation."
      return (existingDocs !! 0)
    else do
      putStrLn "Documentation not found. Generating with cabal haddock..."
      putStrLn "This may take a few minutes..."
      
      -- Use --haddock-all to generate all documentation
      (genExitCode, _, genStderr) <- System.Process.readProcessWithExitCode "cabal" 
        ["haddock", "--haddock-executables", "--haddock-all"] ""
      
      case genExitCode of
        ExitSuccess -> do
          -- Try to find the generated documentation
          (_, newStdout, _) <- System.Process.readProcessWithExitCode "find" 
            ["dist-newstyle", "-name", "index.html", "-path", "*/bird/*"] ""
          
          let newDocs = lines newStdout
          if not (null newDocs) && not (null (newDocs !! 0))
            then do
              putStrLn "Documentation generated successfully."
              return (newDocs !! 0)
            else do
              IO.hPutStrLn IO.stderr $ r ++ "Error: Could not find generated documentation." ++ reset
              IO.hPutStrLn IO.stderr "The documentation might be in a different location."
              IO.hPutStrLn IO.stderr "Try running manually: cabal haddock --haddock-executables"
              exitWith (ExitFailure 1)
        
        ExitFailure code -> do
          IO.hPutStrLn IO.stderr $ r ++ "Error: Documentation generation failed with code " ++ show code ++ reset
          IO.hPutStrLn IO.stderr genStderr
          IO.hPutStrLn IO.stderr "Try running manually: cabal haddock --haddock-executables"
          exitWith (ExitFailure 1)
  
  -- Open with default application (xdg-open on Linux, open on macOS)
  putStrLn $ "Opening documentation: " ++ docPath
  _ <- System.Process.rawSystem "xdg-open" [docPath]
  exitSuccess

-- | Command-line options for the Gloom interpreter
data Options = Options
  { optFile    :: Maybe FilePath  -- ^ File to execute (Nothing = error)
  , optCheck   :: Bool            -- ^ Run static analysis only
  , optVerbose :: Bool            -- ^ Enable verbose output
  , optQuiet   :: Bool            -- ^ Suppress informational messages
  , optVersion :: Bool            -- ^ Show version information
  , optDoc     :: Bool            -- ^ Open Haddock documentation
  } deriving (Show)

-- | Parser for command-line options
optionsParser :: Parser Options
optionsParser = Options
  <$> optional (argument str (metavar "FILE" <> help "Gloom source file to execute"))
  <*> switch (long "check" <> short 'c' <> help "Run static analysis only (no execution)")
  <*> switch (long "verbose" <> short 'v' <> help "Enable verbose output")
  <*> switch (long "quiet" <> short 'q' <> help "Suppress informational messages")
  <*> switch (long "version" <> help "Show version information")
  <*> switch (long "doc" <> help "Open Haddock documentation in default browser")

-- | Parser info with program description
opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Gloom language interpreter - A modern interpreted language with static analysis"
  <> header "gloom - Two-phase interpreter with type checking"
  )

-- | Main entry point - handles CLI arguments and orchestrates execution
--
-- Supports multiple modes:
--
-- * Normal execution: runs the specified file
-- * Check mode (--check): performs static analysis only
-- * Verbose mode (--verbose): shows detailed execution information
-- * Quiet mode (--quiet): minimal output
--
-- The execution follows a three-step model:
--
-- 1. Parse and analyze (type checking, const validation, build symbol tables)
-- 2. Check for unresolved function calls (mutual recursion validation)
-- 3. Find and execute 'main' function (unless --check is specified)
--
-- Exit codes:
-- * 0: Success
-- * 1: Static analysis failure
-- * 2: Runtime execution failure
-- * 3: Entry point not found
main :: IO ()
main = do
  -- Fix portability issue #2: Force UTF-8 encoding on all I/O handles
  -- This ensures consistent behavior across Windows/Linux/macOS
  IO.hSetEncoding IO.stdout IO.utf8
  IO.hSetEncoding IO.stderr IO.utf8
  IO.hSetEncoding IO.stdin IO.utf8
  
  options <- execParser opts
  
  -- Handle --doc flag
  when (optDoc options) openDocumentation
  
  -- Handle --version flag
  when (optVersion options) $ do
    putStrLn "Gloom v0.1.0 (experimental)"
    putStrLn "Copyright (c) João, 2024-2025"
    exitSuccess
  
  -- Get file path or exit with error
  filePath <- case optFile options of
    Just fp -> return fp
    Nothing -> do
      -- Professional CLI error: no input file specified
      IO.hPutStrLn IO.stderr $ "\n" ++ y ++ "error[E0001]: " ++ bold ++ "no input file specified" ++ reset
      IO.hPutStrLn IO.stderr $ "  " ++ b ++ "--> " ++ reset ++ "<command-line>"
      IO.hPutStrLn IO.stderr $ "   " ++ b ++ "|" ++ reset
      IO.hPutStrLn IO.stderr $ "   " ++ b ++ "= " ++ reset ++ "note: the compiler requires a source file to process"
      IO.hPutStrLn IO.stderr $ "   " ++ b ++ "= " ++ reset ++ "help: provide a Gloom source file as an argument"
      IO.hPutStrLn IO.stderr   ""
      IO.hPutStrLn IO.stderr   "Usage:"
      IO.hPutStrLn IO.stderr $ "  " ++ c ++ "gloom <file.gloom>" ++ reset
      IO.hPutStrLn IO.stderr   ""
      IO.hPutStrLn IO.stderr   "Examples:"
      IO.hPutStrLn IO.stderr $ "  " ++ c ++ "gloom hello.gloom" ++ reset ++ "        # Execute file"
      IO.hPutStrLn IO.stderr $ "  " ++ c ++ "gloom --check hello.gloom" ++ reset ++ " # Static analysis only"
      IO.hPutStrLn IO.stderr   ""
      IO.hPutStrLn IO.stderr $ "For more information, try " ++ c ++ "'gloom --help'" ++ reset
      exitWith (ExitFailure 1)
  
  let verbose = optVerbose options
  let quiet = optQuiet options
  
  -- Phase 1: Static Analysis
  unless quiet $ putStrLn $ "Running: " ++ filePath
  when verbose $ putStrLn "Phase 1: Static Analysis (Analysing)"
  
  tokens1 <- getTokens filePath
  let analysingState = freshState
  result1 <- runParserT (program >> getState) analysingState filePath tokens1
  
  case result1 of
    Left err -> do
      -- Flush stdout first to ensure "Running:" message appears before error
      IO.hFlush IO.stdout
      IO.hPutStrLn IO.stderr "\nSTATIC ANALYSIS FAILED:"
      -- Extract just the custom error message, skip parser context
      let errorMsg = show err
          errorLines = lines errorMsg
          -- Find where the actual type error starts (after "unexpected/expecting")
          customError = unlines $ dropWhile (\line -> 
            not ("Type Error:" `isInfixOf` line || 
                 "Error:" `isInfixOf` line)
            ) errorLines
      -- If we found a custom error, show only that; otherwise show everything
      if null customError
        then IO.hPrint IO.stderr err
        else IO.hPutStr IO.stderr customError
      exitWith (ExitFailure 1)
    Right finalState1 -> do
      when verbose $ putStrLn "Static analysis passed"
      
      -- Check for unresolved function calls
      case checkUnresolvedDebits finalState1 of
        Just debitError -> do
          IO.hPutStrLn IO.stderr "UNRESOLVED FUNCTION CALLS:"
          IO.hPutStrLn IO.stderr debitError
          exitWith (ExitFailure 1)
        Nothing -> do
          -- If --check flag is set, stop here
          when (optCheck options) $ do
            unless quiet $ putStrLn "Check completed successfully"
            exitSuccess
          
          -- Look for 'main' function
          case Map.lookup "main" (rtFuns finalState1) of
            Nothing -> do
              -- Professional CLI error: entry point not found
              IO.hPutStrLn IO.stderr $ "\n" ++ y ++ "error[E0601]: " ++ bold ++ "entry point not found" ++ reset
              IO.hPutStrLn IO.stderr $ "  " ++ b ++ "--> " ++ reset ++ "<source>"
              IO.hPutStrLn IO.stderr $ "   " ++ b ++ "|" ++ reset
              IO.hPutStrLn IO.stderr $ "   " ++ b ++ "= " ++ reset ++ "note: the program requires a " ++ c ++ "main" ++ reset ++ " function as an entry point"
              IO.hPutStrLn IO.stderr $ "   " ++ b ++ "= " ++ reset ++ "help: add a " ++ c ++ "main" ++ reset ++ " function to your program"
              IO.hPutStrLn IO.stderr   ""
              IO.hPutStrLn IO.stderr   "Example:"
              IO.hPutStrLn IO.stderr $ "  " ++ c ++ "fn main() {" ++ reset
              IO.hPutStrLn IO.stderr   "    // Your code here"
              IO.hPutStrLn IO.stderr $ "  " ++ c ++ "}" ++ reset
              exitWith (ExitFailure 3)
            Just _ -> do
              -- Phase 2: Execution
              when verbose $ putStrLn "Phase 2: Execution (Running)"
              when verbose $ putStrLn "Entry point found: main()"
              
              tokens2 <- getTokens filePath
              let runningState = finalState1 { rtFlag = Running }
              result2 <- runParserT program runningState filePath tokens2
              
              case result2 of
                Left err -> do
                  IO.hPutStrLn IO.stderr "EXECUTION FAILED:"
                  IO.hPrint IO.stderr err
                  exitWith (ExitFailure 2)
                Right () -> do
                  when verbose $ putStrLn "Program executed successfully"
                  exitSuccess