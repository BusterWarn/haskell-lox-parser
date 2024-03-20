module Interpreter (interpret) where

import AbstractSyntaxTree
import Data.List (isSuffixOf)
import Parser (parse)
import Scanner (scanTokens)
import Tokens

import Control.Exception (evaluate)
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
type Variable = (Maybe LoxValue, Mutability)
type Environment = [Map.Map String Variable]

-- Declare a new variable or redeclare an existing one
define :: String -> Maybe LoxValue -> TokenType -> Environment -> Environment
define name value CONST (current : outer) = Map.insert name (value, Immutable) current : outer
define name value _ (current : outer) = Map.insert name (value, Mutable) current : outer

-- Assign value to already existing variable.
assign :: String -> LoxValue -> Environment -> Either LoxRuntimeError Environment
assign name _ [] = Left $ LoxRuntimeError $ "Undefined variable '" ++ name ++ "'."
assign name value (current : outer) =
  case Map.lookup name current of
    Just (_, Mutable) -> Right $ Map.insert name (Just value, Mutable) current : outer
    Just (Nothing, Immutable) -> Right $ Map.insert name (Just value, Immutable) current : outer
    Just (Just _, Immutable) -> Left $ LoxRuntimeError $ "Attempted to reassign constant '" ++ name ++ "'."
    Nothing -> do
      newOuter <- assign name value outer
      Right $ current : newOuter

-- Get a variable's value, returning Either LoxRuntimeError LoxValue
getVar :: String -> Environment -> Either LoxRuntimeError Variable
getVar name [] = Left $ LoxRuntimeError ("Undefined variable '" ++ name ++ "'.")
getVar name (current : outer) =
  case Map.lookup name current of
    Just variable -> Right variable
    Nothing -> getVar name outer

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
   in interpretStmts stmts [Map.empty]

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
  (newEnv, _) <- evaluateExpr expr env
  Right (newEnv, [])
evaluateStmt (PrintStmt expr) env = do
  (newEnv, printResult) <- evaluateExpr expr env
  Right (newEnv, [show printResult])
evaluateStmt (BlockStmt stmts) env = evaluateBlockStmts stmts (Map.empty : env) []
 where
  evaluateBlockStmts [] tempEnv acc = Right (tail tempEnv, acc)
  evaluateBlockStmts (s : ss) tempEnv acc = do
    (newy, newStrings) <- evaluateStmt s tempEnv
    evaluateBlockStmts ss newy (acc ++ newStrings)
evaluateStmt (IfStmt expr stmt maybeStmt) env = do
  (newEnv, exprValue) <- evaluateExpr expr env
  if isTruthy exprValue
    then evaluateStmt stmt newEnv
    else case maybeStmt of
      Just elseStmt -> evaluateStmt elseStmt newEnv
      _ -> Right (newEnv, [])
evaluateStmt (WhileStmt expr stmt) env = undefined
evaluateStmt (VarDeclStmt tokenType (TOKEN _ _ (ID name) _) EmptyExpr) env = do
  Right (define name Nothing tokenType env, [])
evaluateStmt (VarDeclStmt tokenType (TOKEN _ _ (ID name) _) expr) env = do
  (newEnv, val) <- evaluateExpr expr env
  Right (define name (Just val) tokenType newEnv, [])
evaluateStmt (ErrorStmt expr) env = undefined

evaluateExpr :: Expr -> Environment -> Either LoxRuntimeError (Environment, LoxValue)
-- GroupingExpr
evaluateExpr (GroupingExpr expr) env = evaluateExpr expr env
-- LiteralExpr
evaluateExpr (LiteralExpr (TOKEN _ _ literal _)) env =
  case literal of
    NUM value -> Right (env, LoxNumber value)
    STR value -> Right (env, LoxString value)
    TRUE_LIT -> Right (env, LoxBool True)
    FALSE_LIT -> Right (env, LoxBool False)
    NIL_LIT -> Right (env, LoxNil)
    ID name -> do
      (result, _) <- getVar name env
      case result of
        Just value -> Right (env, value)
        Nothing -> Left $ LoxRuntimeError $ "Tried to evaluate variable without assignment: '" ++ name ++ "'"
    _ -> Left $ LoxRuntimeError $ "Unknown literal: " ++ show literal
-- UnaryExpr
evaluateExpr (UnaryExpr (TOKEN unary _ _ _) expr) env = do
  (envAfterRight, right) <- evaluateExpr expr env
  case (unary, right) of
    (MINUS, LoxNumber r) -> Right (envAfterRight, LoxNumber (-r))
    (BANG, _) -> Right (envAfterRight, LoxBool . not $ isTruthy right)
-- BinaryExpr
evaluateExpr (BinaryExpr leftExpr (TOKEN OR _ _ _) rightExpr) env = do
  (envAfterLeft, leftVal) <- evaluateExpr leftExpr env
  if isTruthy leftVal
    then Right (envAfterLeft, leftVal)
    else evaluateExpr rightExpr envAfterLeft
evaluateExpr (BinaryExpr leftExpr (TOKEN AND _ _ _) rightExpr) env = do
  (envAfterLeft, leftVal) <- evaluateExpr leftExpr env
  if not . isTruthy $ leftVal
    then Right (envAfterLeft, leftVal)
    else evaluateExpr rightExpr envAfterLeft
evaluateExpr (BinaryExpr leftExpr token rightExpr) env = do
  (envAfterLeft, leftVal) <- evaluateExpr leftExpr env
  (envAfterRight, rightVal) <- evaluateExpr rightExpr envAfterLeft
  case token of
    (TOKEN STAR _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right (envAfterRight, LoxNumber (l * r))
      _ -> Left $ LoxRuntimeError $ "Binary operand '*' requires number on left and right side. Actual: " ++ show leftVal ++ " * " ++ show rightVal
    (TOKEN SLASH _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber _, LoxNumber 0) -> Left $ LoxRuntimeError $ "Division by zero: " ++ show leftVal ++ " / " ++ show rightVal
      (LoxNumber l, LoxNumber r) -> Right (envAfterRight, LoxNumber (l / r))
      _ -> Left $ LoxRuntimeError $ "Binary operand '/' requires number on left and right side. Actual: " ++ show leftVal ++ " / " ++ show rightVal
    (TOKEN MINUS _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right (envAfterRight, LoxNumber (l - r))
      _ -> Left $ LoxRuntimeError $ "Binary operand '-' requires number on left and right side. Actual: " ++ show leftVal ++ " - " ++ show rightVal
    (TOKEN PLUS _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right (envAfterRight, LoxNumber (l + r))
      (LoxString l, LoxString r) -> Right (envAfterRight, LoxString (r ++ l))
      (LoxNumber _, _) -> Left $ LoxRuntimeError $ "Binary operand '+' requires number on left and right side. Actual: " ++ show leftVal ++ " + " ++ show rightVal
      (_, LoxNumber _) -> Left $ LoxRuntimeError $ "Binary operand '+' requires number on left and right side. Actual: " ++ show leftVal ++ " + " ++ show rightVal
      (LoxString _, _) -> Left $ LoxRuntimeError $ "Binary operand '+' requires string on left and right side. Actual: " ++ show leftVal ++ " + " ++ show rightVal
      (_, LoxString _) -> Left $ LoxRuntimeError $ "Binary operand '+' requires string on left and right side. Actual: " ++ show leftVal ++ " + " ++ show rightVal
      _ -> Left $ LoxRuntimeError $ "Binary operand '+' does not support: " ++ show leftVal ++ " + " ++ show rightVal
    (TOKEN EQUAL_EQUAL _ _ _) -> Right (envAfterRight, LoxBool (leftVal == rightVal))
    (TOKEN BANG_EQUAL _ _ _) -> Right (envAfterRight, LoxBool (leftVal /= rightVal))
    (TOKEN GREATER _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right (envAfterRight, LoxBool (l > r))
      _ -> Left $ LoxRuntimeError $ "Binary operand '>' requires numbers on left and right side. Actual: " ++ show leftVal ++ " > " ++ show rightVal
    (TOKEN GREATER_EQUAL _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right (envAfterRight, LoxBool (l >= r))
      _ -> Left $ LoxRuntimeError $ "Binary operand '>=' requires numbers on left and right side. Actual: " ++ show leftVal ++ " >= " ++ show rightVal
    (TOKEN LESS _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right (envAfterRight, LoxBool (l < r))
      _ -> Left $ LoxRuntimeError $ "Binary operand '<' requires numbers on left and right side. Actual: " ++ show leftVal ++ " < " ++ show rightVal
    (TOKEN LESS_EQUAL _ _ _) -> case (leftVal, rightVal) of
      (LoxNumber l, LoxNumber r) -> Right (envAfterRight, LoxBool (l <= r))
      _ -> Left $ LoxRuntimeError $ "Binary operand '<=' requires numbers on left and right side. Actual: " ++ show leftVal ++ " <= " ++ show rightVal
    (TOKEN OR _ _ _) ->
      if isTruthy leftVal
        then Right (envAfterRight, leftVal)
        else Right (envAfterRight, rightVal)
    (TOKEN AND _ _ _) ->
      if not $ isTruthy leftVal
        then Right (envAfterRight, leftVal)
        else Right (envAfterRight, rightVal)
    _ -> Left . LoxRuntimeError $ "Unsupported binary operator: '" ++ show token ++ "'."
-- AssignExpr
evaluateExpr (AssignExpr (TOKEN _ _ (ID name) _) expr) env = do
  (envAfterEval, newValue) <- evaluateExpr expr env
  envAfterAssign <- assign name newValue envAfterEval
  Right (envAfterAssign, newValue)
-- EmptyExpr TODO: Not sure if this is correct
evaluateExpr EmptyExpr env = Right (env, LoxNil)

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool theTruth) = theTruth
isTruthy _ = True
