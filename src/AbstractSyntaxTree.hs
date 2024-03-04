module AbstractSyntaxTree where

import Tokens

data Stmt
  = ExprStmt Expr
  | PrintStmt Expr
  | BlockStmt [Stmt]
  | IfStmt Expr Stmt (Maybe Stmt) -- Condition, thenBranch, elseBranch (optional)
  | VarDeclStmt Token Expr
  | ErrorStmt Expr

instance Show Stmt where
  show (ExprStmt expr) = show expr ++ ";"
  show (PrintStmt expr) = "print " ++ show expr ++ ";"
  show (BlockStmt stmts) = "{ " ++ concatMap (\stmt -> show stmt ++ " ") stmts ++ "}"
  show (IfStmt condition thenStmt elseStmt) =
    "if ("
      ++ show condition
      ++ ") "
      ++ show thenStmt
      ++ maybe "" (\e -> " else " ++ show e) elseStmt
  show (VarDeclStmt token EmptyExpr) = "V DEC -> " ++ show token ++ ";"
  show (VarDeclStmt token expr) = "V DEC -> " ++ show token ++ " = " ++ show expr ++ ";"
  show (ErrorStmt err) = show err

data Expr
  = LiteralExpr Token
  | AssignExpr Token Expr
  | UnaryExpr Token Expr
  | BinaryExpr Expr Token Expr
  | GroupingExpr Expr
  | ErrorExpr LoxParseError
  | EmptyExpr

instance Show Expr where
  show (LiteralExpr token) = show token
  show (UnaryExpr operator right) = "(" ++ show operator ++ " " ++ show right ++ ")"
  show (BinaryExpr left operator right) = "(" ++ show left ++ " " ++ show operator ++ " " ++ show right ++ ")"
  show (GroupingExpr expr) = "(" ++ show expr ++ ")"
  show (AssignExpr token expr) = show token ++ " = " ++ show expr
  show (ErrorExpr err) = show err
  show EmptyExpr = ""

newtype Statements = Statements [Stmt]

instance Show Statements where
  show (Statements stmts) = concatMap show stmts

data LoxParseError = LoxParseError String Token

instance Show LoxParseError where
  show (LoxParseError errMsg token) =
    describeToken token ++ ": " ++ errMsg
   where
    describeToken t@(TOKEN _ _ _ l)
      | isEOF t = "Line " ++ show l ++ ", reached <EOF>"
      | otherwise = "Line " ++ show l ++ ", near '" ++ show t ++ "'"
