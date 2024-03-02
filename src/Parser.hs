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
   in go left restFromLeft
 where
  go l rest@(t : ts)
    | isEquality t =
        let (right, restFromRight) = comparison ts
         in go (Node t l right) restFromRight
    | otherwise = (l, rest)

comparison :: [Token] -> (Ast, [Token])
comparison tokens =
  let (left, restFromLeft) = term tokens
   in go left restFromLeft
 where
  go l rest@(t : ts)
    | isComparision t =
        let (right, restFromRight) = term ts
         in go (Node t l right) restFromRight
    | otherwise = (l, rest)

term :: [Token] -> (Ast, [Token])
term tokens =
  let (left, restFromLeft) = factor tokens
   in go left restFromLeft
 where
  go l rest@(t : ts)
    | isBinaryAdditive t =
        let (right, restFromRight) = factor ts
         in go (Node t l right) restFromRight
    | otherwise = (l, rest)

factor :: [Token] -> (Ast, [Token])
factor [] = error "Empty list of Tokens!"
factor tokens =
  let (left, restFromLeft) = unary tokens
   in go left restFromLeft
 where
  go l rest@(t : ts)
    | isBinaryMultiplicative t =
        let (right, restFromRight) = unary ts
         in go (Node t l right) restFromRight
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
primary (t : ts)
  | isLiteral t = (Node t EmptyAst EmptyAst, ts)
  | otherwise = error "I don't know how to deal with '(' or ')'"

consume :: [Token] -> Token -> String
consume = undefined
