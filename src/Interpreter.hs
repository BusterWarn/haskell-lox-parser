module Interpreter (interpret) where

import AbstractSyntaxTree
import Data.List (isSuffixOf)
import Parser (parse)
import Scanner (scanTokens)
import Tokens

import qualified Data.Map as Map

data LoxValue
  = LoxNumber Float
  | LoxString String
  | LoxBool Bool
  | LoxNil
  deriving (Eq)

instance Show LoxValue where
  show LoxNil = "nil"
  show (LoxNumber n) =
    let text = show n
     in if ".0" `isSuffixOf` text then take (length text - 2) text else text
  show (LoxString s) = reverse s
  show (LoxBool b) = if b then "true" else "false"

data LoxRuntimeError = LoxRuntimeError String deriving (Show, Eq)

data Mutability = Mutable | Immutable deriving (Eq, Show)
type Variable = (LoxValue, Mutability)
type Environment = Map.Map String Variable

-- Define a new variable or update an existing one
define :: String -> LoxValue -> TokenType -> Environment -> Environment
define name value CONST = Map.insert name (value, Immutable)
define name value _ = Map.insert name (value, Mutable)

-- Get a variable's value, returning Either LoxRuntimeError LoxValue
getVar :: String -> Environment -> Either LoxRuntimeError LoxValue
getVar name env =
  case Map.lookup name env of
    Just (value, _) -> Right value
    Nothing -> Left $ LoxRuntimeError ("Undefined variable '" ++ name ++ "'.")

{- |
  'interpret' - Interprets Lox language code.

  Input:
    - 'code :: String' - Source code of the program.

  Output:
    - 'Either LoxRuntimeError (Environment, [String])' - Either a runtime error or the final environment and STDOUT from prints.
-}
interpret :: String -> Either LoxRuntimeError (Environment, [String])
interpret code =
  let (Ast stmts) = parse . scanTokens $ code
   in interpretStmts stmts Map.empty

{- |
  'interpretStmts' - Recursively evaluates a list of statements, updating the environment.

  Input:
    - 'stmts :: [Stmt]' - Statements to interpret.
    - 'env :: Environment' - Initial environment for statement evaluation.

  Output:
    - 'Either LoxRuntimeError (Environment, [String])' - Either a runtime error or the final environment and STDOUT from prints.
-}
interpretStmts :: [Stmt] -> Environment -> Either LoxRuntimeError (Environment, [String])
interpretStmts [] env = Right (env, [])
interpretStmts (stmt : rest) env =
  case evaluateStmt stmt env of
    Left err -> Left err
    Right (newEnv, output) ->
      case interpretStmts rest newEnv of
        Left err -> Left err
        Right (finalEnv, outputs) -> Right (finalEnv, output ++ outputs)

evaluateStmt :: Stmt -> Environment -> Either LoxRuntimeError (Environment, [String])
evaluateStmt (ExprStmt expr) env = do
  _ <- evaluateExpr expr env
  Right (env, [])
evaluateStmt (PrintStmt expr) env = do
  printResult <- evaluateExpr expr env
  Right (env, [show printResult])
evaluateStmt (BlockStmt stmts) env = undefined
evaluateStmt (IfStmt expr stmt maybeStmt) env = undefined
evaluateStmt (WhileStmt expr stmt) env = undefined
evaluateStmt (VarDeclStmt tokenType (TOKEN _ _ (ID name) _) expr) env = do
  val <- evaluateExpr expr env
  Right (define name val tokenType env, [])
evaluateStmt (ErrorStmt expr) env = undefined

evaluateExpr :: Expr -> Environment -> Either LoxRuntimeError LoxValue
evaluateExpr (GroupingExpr expr) env = evaluateExpr expr env
evaluateExpr (LiteralExpr (TOKEN _ _ literal _)) env =
  case literal of
    NUM value -> Right $ LoxNumber value
    STR value -> Right $ LoxString value
    TRUE_LIT -> Right $ LoxBool True
    FALSE_LIT -> Right $ LoxBool False
    NIL_LIT -> Right LoxNil
    ID value -> getVar value env
    _ -> Left $ LoxRuntimeError $ "Unknown literal: " ++ show literal
evaluateExpr (UnaryExpr (TOKEN unary _ _ _) expr) env = do
  right <- evaluateExpr expr env
  case (unary, right) of
    (MINUS, LoxNumber r) -> Right $ LoxNumber (-r)
    (BANG, LoxBool r) -> Right $ LoxBool (not r)
evaluateExpr (BinaryExpr leftExpr token rightExpr) env = do
  leftVal <- evaluateExpr leftExpr env
  rightVal <- evaluateExpr rightExpr env
  case token of
    (TOKEN STAR _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right $ LoxNumber (l * r)
      _ -> Left $ LoxRuntimeError $ "Binary operand '*' requires number on left and right side. Actual: " ++ show leftVal ++ " * " ++ show rightVal
    (TOKEN SLASH _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber _, LoxNumber 0) -> Left $ LoxRuntimeError $ "Division by zero: " ++ show leftVal ++ " / " ++ show rightVal
      (LoxNumber l, LoxNumber r) -> Right $ LoxNumber (l / r)
      _ -> Left $ LoxRuntimeError $ "Binary operand '/' requires number on left and right side. Actual: " ++ show leftVal ++ " / " ++ show rightVal
    (TOKEN MINUS _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right $ LoxNumber (l - r)
      _ -> Left $ LoxRuntimeError $ "Binary operand '-' requires number on left and right side. Actual: " ++ show leftVal ++ " - " ++ show rightVal
    (TOKEN PLUS _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right $ LoxNumber (l + r)
      (LoxString l, LoxString r) -> Right $ LoxString (r ++ l)
      (LoxNumber l, _) -> Left $ LoxRuntimeError $ "Binary operand '+' requires number on left and right side. Actual: " ++ show leftVal ++ " + " ++ show rightVal
      (_, LoxNumber r) -> Left $ LoxRuntimeError $ "Binary operand '+' requires number on left and right side. Actual: " ++ show leftVal ++ " + " ++ show rightVal
      (LoxString l, _) -> Left $ LoxRuntimeError $ "Binary operand '+' requires string on left and right side. Actual: " ++ show leftVal ++ " + " ++ show rightVal
      (_, LoxString r) -> Left $ LoxRuntimeError $ "Binary operand '+' requires string on left and right side. Actual: " ++ show leftVal ++ " + " ++ show rightVal
      _ -> Left $ LoxRuntimeError $ "Binary operand '+' does not support: " ++ show leftVal ++ " + " ++ show rightVal
    _ -> Left $ LoxRuntimeError "Unsupported binary operator."
