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
      result = consume restAfterExppresion SEMICOLON "Expect ';' after expression." -- TODO: fix bug here
   in case result of
        Left restAfterConsume -> (DeclExpr expr idToken, restAfterConsume)
        Right err -> (ErrorExpr err, restAfterExppresion)
varDeclaration (idToken@(TOKEN IDENTIFIER _ _ _) : (TOKEN SEMICOLON _ _ _) : rest) = (DeclExpr EmptyExpr idToken, rest)
varDeclaration tokens@(t : _) = (ErrorExpr $ LoxParseError ("Expect '=' or ';' after identifier, got: " ++ show t) t, tokens)

statement [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
statement tokens@((TOKEN tokenType _ _ _) : ts)
  | tokenType == PRINT = printStatement ts
  | tokenType == LEFT_BRACE = block ts
  | tokenType == IF = ifStatement ts
  | otherwise = expressionStatement tokens

ifStatement :: [Token] -> (Expr, [Token])
ifStatement [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
ifStatement tokens =
  let resultAfterLeftParen = consume tokens LEFT_PAREN "Expect '(' after 'if'."
   in case resultAfterLeftParen of
        Right err -> (ErrorExpr err, tokens)
        Left restAfterConsumeLeftParen ->
          let (condition, restAfterExpr) = expression restAfterConsumeLeftParen
              resultAfterRightParen = consume restAfterExpr RIGHT_PAREN "Expect ')' after if expression."
           in case resultAfterRightParen of
                Right err -> (ErrorExpr err, restAfterExpr)
                Left restAfterRightParen ->
                  let (ifStmt, restAfterStatement@(t : restAfterElse)) = statement restAfterRightParen
                      (elseStmt, finalRest) = case t of
                        (TOKEN ELSE _ _ _) -> statement restAfterElse
                        _ -> (EmptyExpr, restAfterStatement)
                   in (IfExpr condition ifStmt elseStmt, finalRest)

block :: [Token] -> (Expr, [Token])
block [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
block tokens = blockHelper tokens []
 where
  blockHelper [] _ = error "Internal parser error. Nothing to parse, but parser expects statement in block."
  blockHelper rest@(t@(TOKEN tokenType _ _ _) : ts) tokensAcc
    | tokenType == EOF = (ErrorExpr $ LoxParseError "Expect '}' after block" t, rest)
    | tokenType == RIGHT_BRACE = (Block $ reverse tokensAcc, ts)
    | otherwise =
        let (newStatement, statementRest) = declaration rest
         in blockHelper statementRest (newStatement : tokensAcc)

printStatement :: [Token] -> (Expr, [Token])
printStatement [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
printStatement tokens =
  let (expr, rest) = expression tokens
      result = consume rest SEMICOLON "Expect ';' after value."
   in case result of
        Left restAfterConsume -> (PrintExpr expr, restAfterConsume)
        Right err -> (ErrorExpr err, rest)

expressionStatement :: [Token] -> (Expr, [Token])
expressionStatement [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
expressionStatement tokens =
  let (expr, rest) = expression tokens
      result = consume rest SEMICOLON "Expect ';' after expression."
   in case result of
        Left restAfterConsume -> (expr, restAfterConsume)
        Right err -> (ErrorExpr err, rest)

expression :: [Token] -> (Expr, [Token])
expression [] = error "Empty list of Tokens!"
expression tokens = assignment tokens

assignment :: [Token] -> (Expr, [Token])
assignment [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
assignment tokens@(possibleIdentifierToken : _) =
  let (left, restFromLeft) = loxOr tokens
   in case restFromLeft of
        ((TOKEN EQUAL _ _ _) : rest) ->
          if isTokenLValue possibleIdentifierToken
            then
              let (right, restFromRight) = assignment rest
               in (AssignExpr possibleIdentifierToken right, restFromRight)
            else
              let err = ErrorExpr $ LoxParseError ("Invalid assignment to rvalue: '" ++ show possibleIdentifierToken ++ "'") possibleIdentifierToken
                  newRest = synchronize rest
               in (err, newRest)
        _ -> (left, restFromLeft)
 where
  isTokenLValue (TOKEN IDENTIFIER _ _ _) = True
  isTokenLValue _ = False

loxOr :: [Token] -> (Expr, [Token])
loxOr [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
loxOr tokens =
  let (left, restFromLeft) = loxAnd tokens
   in case left of
        err@(ErrorExpr _) -> (err, restFromLeft)
        _ -> matchOr left restFromLeft
 where
  matchOr left rest@(orToken@(TOKEN tokenType _ _ _) : ts)
    | tokenType == OR =
        let (right, restFromRight) = loxAnd ts
         in case right of
              err@(ErrorExpr _) -> (err, restFromRight)
              _ -> matchOr (BinaryExpr left orToken right) restFromRight
    | otherwise = (left, rest)

loxAnd :: [Token] -> (Expr, [Token])
loxAnd [] = (ErrorExpr $ LoxParseError "Empty list of Tokens!" (TOKEN EOF "" NONE 0), [])
loxAnd tokens =
  let (left, restFromLeft) = equality tokens
   in case left of
        err@(ErrorExpr _) -> (err, restFromLeft)
        _ -> matchAnd left restFromLeft
 where
  matchAnd left rest@(andToken@(TOKEN tokenType _ _ _) : ts)
    | tokenType == AND =
        let (right, restFromRight) = equality ts
         in case right of
              err@(ErrorExpr _) -> (err, restFromRight)
              _ -> matchAnd (BinaryExpr left andToken right) restFromRight
    | otherwise = (left, rest)

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
          result = consume rest RIGHT_PAREN "Expect ')' after expression."
       in case result of
            Left restAfterConsume -> (GroupingExpr left, restAfterConsume)
            Right err -> (ErrorExpr err, rest)
  | otherwise = (ErrorExpr $ LoxParseError "Unexpected Character" t, ts)

consume :: [Token] -> TokenType -> String -> Either [Token] LoxParseError
consume [] _ errorMessage = Right $ LoxParseError errorMessage (TOKEN EOF "" NONE 0)
consume (actual@(TOKEN actualTokenType _ _ _) : ts) expectedTokenType errorMessage
  | actualTokenType == expectedTokenType = Left ts
  | otherwise = Right $ LoxParseError errorMessage actual

synchronize :: [Token] -> [Token]
synchronize [] = []
synchronize tokens@((TOKEN tokenType _ _ _) : ts)
  | tokenType == SEMICOLON = ts
  | tokenType == EOF = tokens
  | otherwise = synchronize ts
