module Interpereter (interperet, evaluateExpr) where

import AbstractSyntaxTree
import Parser
import Scanner
import Tokens

interperet :: String -> Float
interperet code =
  let ast@(Ast stmts) = parse . scanTokens $ code
      results = map evaluateStmt stmts
   in sum results

evaluateStmt :: Stmt -> Float
evaluateStmt (ExprStmt expr) = evaluateExpr expr
evaluateStmt (PrintStmt expr) = undefined
evaluateStmt (BlockStmt stmts) = undefined
evaluateStmt (IfStmt expr stmt maybeStmt) = undefined
evaluateStmt (WhileStmt expr stmt) = undefined
evaluateStmt (VarDeclStmt tokenType token expr) = undefined
evaluateStmt (ErrorStmt expr) = undefined

evaluateExpr :: Expr -> Float
evaluateExpr (LiteralExpr token@(TOKEN _ _ (NUM value) _)) = value
evaluateExpr (GroupingExpr expr) = evaluateExpr expr
evaluateExpr (UnaryExpr (TOKEN unary _ _ _) expr) =
  let right = evaluateExpr expr
   in case unary of
        MINUS -> -right
        _ -> 0
evaluateExpr (BinaryExpr leftExpr (TOKEN binary _ _ _) rightExpr) =
  let left = evaluateExpr leftExpr
      right = evaluateExpr rightExpr
   in case binary of
        MINUS -> left - right
        PLUS -> left + right
        SLASH -> left / right
        STAR -> left * right
evaluateExpr _ = 0
