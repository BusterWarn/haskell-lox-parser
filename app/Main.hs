import Control.Monad (when)
import FileInterpreter (interpretFile)
import System.Environment (getArgs)

{- |
  'main' - Entry point for the Lox interpreter. It processes command-line arguments and interprets the specified file.

  Input:
    - None directly; expects command-line arguments.

  Output:
    - None directly; outputs are printed to stdout or stderr.
-}
main :: IO ()
main = do
  args <- getArgs
  when (null args)
    $ putStrLn "Usage: runhaskell your_file.lox <filename>"
  case args of
    [filename] -> do
      outputs <- interpretFile filename
      mapM_ putStrLn outputs
    _ -> putStrLn "Error: A single filename argument is expected."
