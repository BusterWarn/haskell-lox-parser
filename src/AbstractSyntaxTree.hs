module AbstractSyntaxTree where

import Tokens

data Expr
  = LiteralExpr Token
  | UnaryExpr Token Expr
  | BinaryExpr Expr Token Expr
  | GroupingExpr Expr
  | ErrorExpr LoxParseError
  | PrintExpr Expr

instance Show Expr where
  show (LiteralExpr token) = show token
  show (UnaryExpr operator right) = "(" ++ show operator ++ " " ++ show right ++ ")"
  show (BinaryExpr left operator right) = "(" ++ show left ++ " " ++ show operator ++ " " ++ show right ++ ")"
  show (GroupingExpr expr) = "(" ++ show expr ++ ")"
  show (ErrorExpr err) = show err
  show (PrintExpr expr) = "print " ++ show expr

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
