module AbstractSyntaxTree where

import Tokens

data Ast = EmptyAst | Node Token Ast Ast

instance Show Ast where
  show EmptyAst = ""
  show (Node value EmptyAst EmptyAst) = show value
  show (Node value EmptyAst right) = "(" ++ show value ++ show right ++ ")"
  show (Node value left right)
    | isBinary value = "(" ++ show left ++ " " ++ show value ++ " " ++ show right ++ ")"
    | otherwise = show value ++ " " ++ show left ++ " " ++ show right
