module Parser (parse) where

import AbstractSyntaxTree
import Debug.Trace (trace)
import Scanner
import Tokens

parse :: [Token] -> Statements
parse [] = error "Empty list of Tokens!"
parse tokens
  | not $ isEOF (last tokens) = error $ "Input Tokens does not end with EOF:\n" ++ show tokens ++ "."
  | length tokens == 1 = error "Only <EOF> token was input."
  | otherwise =
      let statements = parseHelper tokens []
          errors = getAllErrors statements
       in if null errors
            then statements
            else error $ "Encountered errors while parsing:\n" ++ unlines (map show errors)

getAllErrors :: Statements -> [LoxParseError]
getAllErrors (Statements exprs) = concatMap getErrorsFromExpr exprs

getErrorsFromExpr :: Expr -> [LoxParseError]
getErrorsFromExpr (ErrorExpr err) = [err]
getErrorsFromExpr (UnaryExpr _ expr) = getErrorsFromExpr expr
getErrorsFromExpr (BinaryExpr left _ right) = getErrorsFromExpr left ++ getErrorsFromExpr right
getErrorsFromExpr (GroupingExpr expr) = getErrorsFromExpr expr
getErrorsFromExpr _ = []

parseHelper :: [Token] -> [Expr] -> Statements
parseHelper [] statements = error $ "Parsing error! Ran out of tokens, but managed to parse:\n" ++ show statements
parseHelper tokens@(t : ts) statementsAcc
  | null ts && isEOF t = Statements $ reverse statementsAcc
  | otherwise =
      let (s, rest) = declaration tokens
          newStatementsAcc = s : statementsAcc
       in case s of
            (ErrorExpr _) ->
              let newRest = synchronize rest
               in parseHelper newRest newStatementsAcc
            _ -> parseHelper rest newStatementsAcc

declaration :: [Token] -> (Expr, [Token])
declaration [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
declaration tokens@(t : ts)
  | isVar t = varDeclaration ts
  | otherwise = statement tokens

varDeclaration :: [Token] -> (Expr, [Token])
varDeclaration [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
varDeclaration (idToken@(TOKEN IDENTIFIER _ _ _) : (TOKEN EQUAL _ _ _) : exprTokens) =
  let (expr, restAfterExppresion) = expression exprTokens
      (restAfterSemicolon, maybeError) = consume restAfterExppresion SEMICOLON "Expect ';' after expression."
   in case maybeError of
        Just err -> (ErrorExpr err, restAfterSemicolon)
        Nothing -> (DeclExpr idToken expr, restAfterSemicolon)
varDeclaration (idToken@(TOKEN IDENTIFIER _ _ _) : (TOKEN SEMICOLON _ _ _) : rest) = (DeclExpr idToken EmptyExpr, rest)
varDeclaration tokens@(t : _) = (ErrorExpr $ LoxParseError ("Expect '=' or ';' after identifier, got: " ++ show t) t, tokens)

statement [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
statement tokens@(t : _)
  | isPrint t = printStatement tokens
  | otherwise = expressionStatement tokens

printStatement :: [Token] -> (Expr, [Token])
printStatement [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
printStatement (t@(TOKEN tokenType _ _ _) : ts)
  | tokenType /= PRINT = error $ "Internal parser error. Expected print, but got: " ++ show t
  | otherwise =
      let (expr, rest) = expression ts
          (restAgain, maybeError) = consume rest SEMICOLON "Expect ';' after value."
       in case maybeError of
            Just err -> (ErrorExpr err, restAgain)
            Nothing -> (PrintExpr expr, restAgain)

expressionStatement :: [Token] -> (Expr, [Token])
expressionStatement [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
expressionStatement tokens =
  let (expr, rest) = expression tokens
      (restAgain, maybeError) = consume rest SEMICOLON "Expect ';' after expression."
   in case maybeError of
        Just err -> (ErrorExpr err, restAgain)
        Nothing -> (expr, restAgain)

expression :: [Token] -> (Expr, [Token])
expression [] = error "Empty list of Tokens!"
expression tokens = equality tokens

equality :: [Token] -> (Expr, [Token])
equality [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
equality tokens =
  let (left, restFromLeft) = comparison tokens
   in case left of
        err@(ErrorExpr _) -> (err, restFromLeft)
        _ -> matchEqualities left restFromLeft
 where
  matchEqualities left rest@(t : ts)
    | isEquality t =
        let (right, restFromRight) = comparison ts
         in case right of
              err@(ErrorExpr _) -> (err, restFromRight)
              _ -> matchEqualities (BinaryExpr left t right) restFromRight
    | otherwise = (left, rest)

comparison :: [Token] -> (Expr, [Token])
comparison [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
comparison tokens =
  let (left, restFromLeft) = term tokens
   in case left of
        err@(ErrorExpr _) -> (err, restFromLeft)
        _ -> matchComparisions left restFromLeft
 where
  matchComparisions left rest@(t : ts)
    | isComparision t =
        let (right, restFromRight) = term ts
         in case right of
              err@(ErrorExpr _) -> (err, restFromRight)
              _ -> matchComparisions (BinaryExpr left t right) restFromRight
    | otherwise = (left, rest)

term :: [Token] -> (Expr, [Token])
term [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
term tokens =
  let (left, restFromLeft) = factor tokens
   in case left of
        err@(ErrorExpr _) -> (err, restFromLeft)
        _ -> matchTerms left restFromLeft
 where
  matchTerms left rest@(t : ts)
    | isBinaryAdditive t =
        let (right, restFromRight) = factor ts
         in case right of
              err@(ErrorExpr _) -> (err, restFromRight)
              _ -> matchTerms (BinaryExpr left t right) restFromRight
    | otherwise = (left, rest)

factor :: [Token] -> (Expr, [Token])
factor [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
factor tokens =
  let (left, restFromLeft) = unary tokens
   in case left of
        err@(ErrorExpr _) -> (err, restFromLeft)
        _ -> matchFactors left restFromLeft
 where
  matchFactors left rest@(t : ts)
    | isBinaryMultiplicative t =
        let (right, restFromRight) = unary ts
         in case right of
              err@(ErrorExpr _) -> (err, restFromRight)
              _ -> matchFactors (BinaryExpr left t right) restFromRight
    | otherwise = (left, rest)

unary :: [Token] -> (Expr, [Token])
unary tokens@(t : ts) =
  if isUnary t
    then
      let (right, rest) = unary ts
       in case right of
            ErrorExpr _ -> (right, rest)
            _ -> (UnaryExpr t right, rest)
    else primary tokens

primary :: [Token] -> (Expr, [Token])
primary [] = error "Empty list of Tokens!"
primary (t@(TOKEN tokenType _ _ _) : ts)
  | isLiteral t = (LiteralExpr t, ts)
  | tokenType == LEFT_PAREN =
      let (left, rest) = expression ts
          (restAgain, maybeError) = consume rest RIGHT_PAREN "Expect ')' after expression."
       in case maybeError of
            Just err -> (ErrorExpr err, restAgain)
            Nothing -> (GroupingExpr left, restAgain)
  | otherwise = (ErrorExpr $ LoxParseError "Unexpected Character" t, ts)

consume :: [Token] -> TokenType -> String -> ([Token], Maybe LoxParseError)
consume [] _ errorMessage =
  ([], Just $ LoxParseError errorMessage (TOKEN EOF "" NONE 0))
consume (actual@(TOKEN actualTokenType _ _ _) : ts) expectedTokenType errorMessage
  | actualTokenType == expectedTokenType = (ts, Nothing)
  | otherwise = (actual : ts, Just $ LoxParseError errorMessage actual)

synchronize :: [Token] -> [Token]
synchronize [] = []
synchronize tokens@((TOKEN tokenType _ _ _) : ts)
  | tokenType == SEMICOLON = ts
  | tokenType == EOF = tokens
  | otherwise = synchronize ts
