module AbstractSyntaxTree where

import Tokens

newtype Ast = Ast [Stmt]

instance Show Ast where
  show (Ast stmts) =
    let baseDeclarations = map show stmts
        count = length baseDeclarations
     in unlines $ show count : baseDeclarations

data Stmt
  = ExprStmt Expr
  | PrintStmt Expr
  | BlockStmt [Stmt]
  | IfStmt Expr Stmt (Maybe Stmt)
  | WhileStmt Expr Stmt
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
  show (WhileStmt condition stmt) = "while (" ++ show condition ++ ")" ++ show stmt
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

data LoxParseError = LoxParseError String Token

instance Show LoxParseError where
  show (LoxParseError errMsg token) =
    describeToken token ++ ": " ++ errMsg
   where
    describeToken t@(TOKEN _ _ _ l)
      | isEOF t = "Line " ++ show l ++ ", reached <EOF>"
      | otherwise = "Line " ++ show l ++ ", near '" ++ show t ++ "'"

-- Collects all parse errors from Ast
getAllErrors :: Ast -> [LoxParseError]
getAllErrors (Ast stmts) = concatMap getErrorsFromStmt stmts

-- Collects all parse errors from a single statement.
getErrorsFromStmt :: Stmt -> [LoxParseError]
getErrorsFromStmt (ExprStmt expr) = getErrorsFromExpr expr
getErrorsFromStmt (PrintStmt expr) = getErrorsFromExpr expr
getErrorsFromStmt (BlockStmt stmts) = concatMap getErrorsFromStmt stmts
getErrorsFromStmt (IfStmt condition thenBranch elseBranch) =
  getErrorsFromExpr condition ++ getErrorsFromStmt thenBranch ++ maybe [] getErrorsFromStmt elseBranch
getErrorsFromStmt (WhileStmt condition stmt) = getErrorsFromExpr condition ++ getErrorsFromStmt stmt
getErrorsFromStmt (VarDeclStmt _ expr) = getErrorsFromExpr expr
getErrorsFromStmt (ErrorStmt expr) = getErrorsFromExpr expr

-- Collects all parse errors from a single expression.
getErrorsFromExpr :: Expr -> [LoxParseError]
getErrorsFromExpr (ErrorExpr err) = [err]
getErrorsFromExpr (UnaryExpr _ expr) = getErrorsFromExpr expr
getErrorsFromExpr (BinaryExpr left _ right) = getErrorsFromExpr left ++ getErrorsFromExpr right
getErrorsFromExpr (GroupingExpr expr) = getErrorsFromExpr expr
getErrorsFromExpr _ = []
