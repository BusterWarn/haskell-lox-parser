module FileInterpreter (interpretFile) where

import Interpreter (interpretAst)
import Parser (parse)
import Scanner (scanTokens)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)

{- |
  'interpretFile' - Reads a Lox file, scans, parses, and interprets its content.

  Input:
    - 'filename :: String' - The path to the Lox source file to be interpreted.

  Output:
    - 'IO [String]' - The output from interpretation, if successful; otherwise, it returns an error message.
-}
interpretFile :: String -> IO [String]
interpretFile filename = do
  result <- tryIOError (readFile filename)
  case result of
    Left err -> do
      hPutStrLn stderr $ "Cannot read file: " ++ filename ++ "\nError: " ++ show err
      return []
    Right contents -> do
      let tokens = scanTokens contents
          ast = parse tokens
      case interpretAst ast of
        (Nothing, prints) -> return prints
        (Just err, prints) -> return (prints ++ [show err])
