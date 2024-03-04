module AbstractSyntaxTree where

import GHC.IO.SubSystem (conditional)
import Tokens

data Expr
  = Block [Expr]
  | IfExpr Expr Expr Expr
  | PrintExpr Expr
  | LiteralExpr Token
  | UnaryExpr Token Expr
  | BinaryExpr Expr Token Expr
  | GroupingExpr Expr
  | DeclExpr Expr Token
  | AssignExpr Token Expr
  | ErrorExpr LoxParseError
  | EmptyExpr

instance Show Expr where
  show (Block exprs) = "{" ++ concatMap showBlockExpr exprs ++ "} "
   where
    showBlockExpr expr = case expr of
      Block _ -> show expr
      _ -> show expr ++ ";"
  show (IfExpr condition ifStmt EmptyExpr) = "if (" ++ show condition ++ ") " ++ show ifStmt
  show (IfExpr condition ifStmt elseStmt) =
    "if (" ++ show condition ++ ") " ++ show ifStmt ++ " else " ++ show elseStmt
  show (PrintExpr expr) = "print " ++ show expr
  show (LiteralExpr token) = show token
  show (UnaryExpr operator right) = "(" ++ show operator ++ " " ++ show right ++ ")"
  show (BinaryExpr left operator right) = "(" ++ show left ++ " " ++ show operator ++ " " ++ show right ++ ")"
  show (GroupingExpr expr) = "(" ++ show expr ++ ")"
  show (DeclExpr EmptyExpr token) = "V DEC -> " ++ show token
  show (DeclExpr expr token) = "V DEC -> " ++ show token ++ " = " ++ show expr
  show (AssignExpr token expr) = show token ++ " = " ++ show expr
  show (ErrorExpr err) = show err
  show EmptyExpr = ""

newtype Statements = Statements [Expr]

instance Show Statements where
  show (Statements []) = ""
  show (Statements exprs) = concatMap showStmt exprs
   where
    showStmt expr@(Block _) = show expr ++ "\n"
    showStmt expr@(IfExpr _ _ _) = show expr ++ "\n"
    showStmt expr = show expr ++ ";\n"

data LoxParseError = LoxParseError String Token

instance Show LoxParseError where
  show (LoxParseError errMsg token) =
    describeToken token ++ ": " ++ errMsg
   where
    describeToken t@(TOKEN _ _ _ l)
      | isEOF t = "Line " ++ show l ++ ", reached <EOF>"
      | otherwise = "Line " ++ show l ++ ", near '" ++ show t ++ "'"
