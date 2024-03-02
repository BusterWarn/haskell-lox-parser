module Parser (parse) where

import AbstractSyntaxTree
import Debug.Trace (trace)
import GHC.Base (undefined)
import Scanner
import Tokens

parse :: [Token] -> Ast
parse [] = error "Empty list of Tokens!"
parse tokens
  | not $ isEOF (last tokens) = error $ "Input Tokens does not end with EOF:\n" ++ show tokens ++ "."
  | length tokens == 1 = error "Only <EOF> token was input."
  | otherwise =
      let (ast, rest) = expression tokens
       in if length rest == 1 && isEOF (head rest)
            then ast
            else error $ "Could not parse. \nParsed AST:" ++ show ast ++ "\nUnable to parse:" ++ show rest

expression :: [Token] -> (Ast, [Token])
expression [] = error "Empty list of Tokens!"
expression tokens = equality tokens

equality :: [Token] -> (Ast, [Token])
equality tokens =
  let (left, restFromLeft) = comparison tokens
   in matchEqualities left restFromLeft
 where
  matchEqualities l rest@(t : ts)
    | isEquality t =
        let (right, restFromRight) = comparison ts
         in matchEqualities (Node t l right) restFromRight
    | otherwise = (l, rest)

comparison :: [Token] -> (Ast, [Token])
comparison tokens =
  let (left, restFromLeft) = term tokens
   in matchComparisions left restFromLeft
 where
  matchComparisions l rest@(t : ts)
    | isComparision t =
        let (right, restFromRight) = term ts
         in matchComparisions (Node t l right) restFromRight
    | otherwise = (l, rest)

term :: [Token] -> (Ast, [Token])
term tokens =
  let (left, restFromLeft) = factor tokens
   in matchTerms left restFromLeft
 where
  matchTerms l rest@(t : ts)
    | isBinaryAdditive t =
        let (right, restFromRight) = factor ts
         in matchTerms (Node t l right) restFromRight
    | otherwise = (l, rest)

factor :: [Token] -> (Ast, [Token])
factor [] = error "Empty list of Tokens!"
factor tokens =
  let (left, restFromLeft) = unary tokens
   in matchFactors left restFromLeft
 where
  matchFactors l rest@(t : ts)
    | isBinaryMultiplicative t =
        let (right, restFromRight) = unary ts
         in matchFactors (Node t l right) restFromRight
    | otherwise = (l, rest)

unary :: [Token] -> (Ast, [Token])
unary tokens@(t : ts) =
  if isUnary t
    then
      let (right, rest) = unary ts
       in (Node t EmptyAst right, rest)
    else
      let res = primary tokens
       in res

primary :: [Token] -> (Ast, [Token])
primary [] = error "Empty list of Tokens!"
primary (t@(TOKEN tokenType _ _ _) : ts)
  | isLiteral t = (Node t EmptyAst EmptyAst, ts)
  | tokenType == LEFT_PAREN =
      let (left, rest) = expression ts
          restAgain = consume rest RIGHT_PAREN "Expect ')' after expression."
       in (left, restAgain)
  | otherwise = error $ "Unknown primary: " ++ show t ++ ". Rest: " ++ show ts

consume :: [Token] -> TokenType -> String -> [Token]
consume (actual@(TOKEN actualTokenType _ _ _) : ts) expectedTokenType errorMessage
  | actualTokenType == expectedTokenType = ts
  | otherwise = error $ parseError actual errorMessage

parseError :: Token -> String -> String
parseError token@(TOKEN _ _ _ line) message
  | isEOF token = "Line: " ++ show line ++ " Reached <EOF>. Parse error: " ++ message
  | otherwise = "Line: " ++ show line ++ " Parse error: " ++ message
