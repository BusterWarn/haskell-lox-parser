module AbstractSyntaxTree where

import Tokens

data Expr
  = LiteralExpr Token
  | UnaryExpr Token Expr
  | BinaryExpr Expr Token Expr
  | GroupingExpr Expr
  | PrintExpr Expr
  | DeclExpr Expr Token
  | AssignExpr Token Expr
  | ErrorExpr LoxParseError
  | EmptyExpr

instance Show Expr where
  show (LiteralExpr token) = show token
  show (UnaryExpr operator right) = "(" ++ show operator ++ " " ++ show right ++ ")"
  show (BinaryExpr left operator right) = "(" ++ show left ++ " " ++ show operator ++ " " ++ show right ++ ")"
  show (GroupingExpr expr) = "(" ++ show expr ++ ")"
  show (PrintExpr expr) = "print " ++ show expr
  show (DeclExpr EmptyExpr token) = "V DEC -> " ++ show token
  show (DeclExpr expr token) = "V DEC -> " ++ show token ++ " = " ++ show expr
  show (AssignExpr token expr) = show token ++ " = " ++ show expr
  show (ErrorExpr err) = show err
  show EmptyExpr = ""

newtype Statements = Statements [Expr]

instance Show Statements where
  show (Statements []) = ""
  show (Statements exprs) = concatMap (\expr -> show expr ++ ";\n") exprs

data LoxParseError = LoxParseError String Token

instance Show LoxParseError where
  show (LoxParseError errMsg token) =
    describeToken token ++ ": " ++ errMsg
   where
    describeToken t@(TOKEN _ _ _ l)
      | isEOF t = "Line " ++ show l ++ ", reached <EOF>"
      | otherwise = "Line " ++ show l ++ ", near '" ++ show t ++ "'"
