module Interpreter (interpret) where

import AbstractSyntaxTree
import AbstractSyntaxTree (LoxParseError (LoxParseError))
import Data.Either (isLeft, rights)
import Data.List (isSuffixOf)
import Parser (parse)
import Scanner (scanTokens)
import Tokens

data LoxValue
  = LoxNumber Float
  | LoxString String
  | LoxBool Bool
  | LoxNil
  deriving (Show, Eq)

data LoxRuntimeError = LoxRuntimeError String deriving (Show, Eq)

interpret :: String -> (Bool, [String])
interpret code =
  let (Ast stmts) = parse . scanTokens $ code
      results = traverse evaluateStmt stmts -- Correctly applies evaluateStmt across stmts
   in case results of
        Left err -> (True, [showError err]) -- Handle the first error encountered
        Right values -> (False, map stringify values) -- Process all successful evaluations
 where
  showError (LoxRuntimeError msg) = "Runtime Error: " ++ msg

stringify :: LoxValue -> String
stringify LoxNil = "nil"
stringify (LoxNumber n) =
  let text = show n
   in if ".0" `isSuffixOf` text then take (length text - 2) text else text
stringify (LoxString s) = reverse s
stringify (LoxBool b) = if b then "true" else "false"

evaluateStmt :: Stmt -> Either LoxRuntimeError LoxValue
evaluateStmt (ExprStmt expr) = evaluateExpr expr
evaluateStmt (PrintStmt expr) = undefined
evaluateStmt (BlockStmt stmts) = undefined
evaluateStmt (IfStmt expr stmt maybeStmt) = undefined
evaluateStmt (WhileStmt expr stmt) = undefined
evaluateStmt (VarDeclStmt tokenType token expr) = undefined
evaluateStmt (ErrorStmt expr) = undefined

evaluateExpr :: Expr -> Either LoxRuntimeError LoxValue
evaluateExpr (GroupingExpr expr) = evaluateExpr expr
evaluateExpr (LiteralExpr (TOKEN _ _ literal _)) =
  case literal of
    NUM value -> Right $ LoxNumber value
    STR value -> Right $ LoxString value
    TRUE_LIT -> Right $ LoxBool True
    FALSE_LIT -> Right $ LoxBool False
    NIL_LIT -> Right LoxNil
    _ -> Left $ LoxRuntimeError $ "Unknown literal: " ++ show literal
evaluateExpr (UnaryExpr (TOKEN unary _ _ _) expr) = do
  right <- evaluateExpr expr
  case (unary, right) of
    (MINUS, LoxNumber r) -> Right $ LoxNumber (-r)
    (BANG, LoxBool r) -> Right $ LoxBool (not r)
evaluateExpr (BinaryExpr leftExpr token rightExpr) = do
  leftVal <- evaluateExpr leftExpr
  rightVal <- evaluateExpr rightExpr
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
