module Interpereter (interperet) where

import AbstractSyntaxTree
import Tokens

interperet :: Ast -> Float
interperet = undefined

evaluate :: Expr -> Float
evaluate (LiteralExpr token@(TOKEN _ _ (NUM value) _)) = value
evaluate (GroupingExpr expr) = evaluate expr
evaluate (UnaryExpr (TOKEN unary _ _ _) expr) =
  let right = evaluate expr
   in case unary of
        MINUS -> -right
        _ -> 0
evaluate (BinaryExpr leftExpr (TOKEN binary _ _ _) rightExpr) =
  let left = evaluate leftExpr
      right = evaluate rightExpr
   in case binary of
        MINUS -> left - right
        PLUS -> left + right
        SLASH -> left / right
        STAR -> left * right
evaluate _ = 0
