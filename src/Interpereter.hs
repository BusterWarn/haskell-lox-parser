module Interpereter (interperet) where

import AbstractSyntaxTree
import Data.List (isSuffixOf)
import Parser (parse)
import Scanner (scanTokens)
import Tokens

data LoxValue
  = LoxNumber Float
  | LoxString String
  | LoxBool Bool
  | LoxNil

interperet :: String -> [String]
interperet code =
  let (Ast stmts) = parse . scanTokens $ code
   in map (stringify . evaluateStmt) stmts

stringify :: LoxValue -> String
stringify LoxNil = "nil"
stringify (LoxNumber n) =
  let text = show n
   in if ".0" `isSuffixOf` text then take (length text - 2) text else text
stringify (LoxString s) = reverse s
stringify (LoxBool b) = if b then "true" else "false"

evaluateStmt :: Stmt -> LoxValue
evaluateStmt (ExprStmt expr) = evaluateExpr expr
evaluateStmt (PrintStmt expr) = undefined
evaluateStmt (BlockStmt stmts) = undefined
evaluateStmt (IfStmt expr stmt maybeStmt) = undefined
evaluateStmt (WhileStmt expr stmt) = undefined
evaluateStmt (VarDeclStmt tokenType token expr) = undefined
evaluateStmt (ErrorStmt expr) = undefined

evaluateExpr :: Expr -> LoxValue
evaluateExpr (LiteralExpr (TOKEN _ _ literal _)) =
  case literal of
    NUM value -> LoxNumber value
    STR value -> LoxString value
    TRUE_LIT -> LoxBool True
    FALSE_LIT -> LoxBool False
    NIL_LIT -> LoxNil
evaluateExpr (GroupingExpr expr) = evaluateExpr expr
evaluateExpr (UnaryExpr (TOKEN unary _ _ _) expr) =
  let right = evaluateExpr expr
   in case (unary, right) of
        (MINUS, LoxNumber r) -> LoxNumber (-r)
        (BANG, LoxBool r) -> LoxBool (not r)
evaluateExpr (BinaryExpr leftExpr (TOKEN binary _ _ _) rightExpr) =
  let left = evaluateExpr leftExpr
      right = evaluateExpr rightExpr
   in case (binary, left, right) of
        (STAR, LoxNumber l, LoxNumber r) -> LoxNumber (l * r)
        (SLASH, LoxNumber l, LoxNumber r) -> LoxNumber (l / r)
        (MINUS, LoxNumber l, LoxNumber r) -> LoxNumber (l - r)
        (PLUS, LoxNumber l, LoxNumber r) -> LoxNumber (l + r)
        (PLUS, LoxString l, LoxString r) -> LoxString (r ++ l)
evaluateExpr _ = LoxNil
