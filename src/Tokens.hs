module Tokens (
  TokenType (..),
  Literal (..),
  Token (..),
  -- Functions
  isEquality,
  isComparision,
  isBinaryAdditive,
  isBinaryMultiplicative,
  isUnary,
  isLiteral,
  isEOF,
  isBinary,
  isVarOrConst,
  isIdentifier,
  isEqual,
) where

-- Datatype TokenType
-- Used for assigning types of tokens
data TokenType = LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE | COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR | BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL | IDENTIFIER | STRING | NUMBER | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR | PRINT | RETURN | SUPER | THIS | TRUE | VAR | CONST | WHILE | EOF deriving (Show, Eq)

-- Datatype Literal
-- Used for storing the actual values of string literals, identifiers, and numbers
data Literal = NONE | STR String | ID String | NUM Float | FALSE_LIT | TRUE_LIT | NIL_LIT deriving (Show, Eq)

-- Datatype Token
-- Represents the tokens the scanner produces and which are used as input for the parser. String is the input that created the token. Int is the rownumber of the token. Remember that rows start at 1.
data Token = TOKEN TokenType String Literal Int

instance Show Token where
  show (TOKEN tokenType stringFrom literal _)
    | tokenType == EOF = "<EOF>"
    | tokenType == AND = "&&"
    | tokenType == OR = "||"
    | literal == NONE = stringFrom
    | otherwise =
        case literal of
          (STR string) -> "\"" ++ reverse string ++ "\""
          (NUM num) -> show num
          (ID str) -> str
          _ -> show literal

isEquality :: Token -> Bool
isEquality (TOKEN token _ _ _) =
  case token of
    EQUAL_EQUAL -> True
    BANG_EQUAL -> True
    _ -> False

isComparision :: Token -> Bool
isComparision (TOKEN token _ _ _) =
  case token of
    GREATER -> True
    GREATER_EQUAL -> True
    LESS -> True
    LESS_EQUAL -> True
    _ -> False

isBinaryAdditive :: Token -> Bool
isBinaryAdditive (TOKEN token _ _ _) =
  case token of
    PLUS -> True
    MINUS -> True
    _ -> False

isBinaryMultiplicative :: Token -> Bool
isBinaryMultiplicative (TOKEN token _ _ _) =
  case token of
    STAR -> True
    SLASH -> True
    _ -> False

isUnary :: Token -> Bool
isUnary (TOKEN token _ _ _) =
  case token of
    BANG -> True
    MINUS -> True
    _ -> False

isLiteral :: Token -> Bool
isLiteral (TOKEN _ _ literal _) =
  case literal of
    NONE -> False
    _ -> True

isEOF :: Token -> Bool
isEOF (TOKEN token _ _ _) =
  case token of
    EOF -> True
    _ -> False

isBinary :: Token -> Bool
isBinary (TOKEN token _ _ _) =
  case token of
    PLUS -> True
    MINUS -> True
    STAR -> True
    SLASH -> True
    _ -> False

isVarOrConst :: Token -> Bool
isVarOrConst (TOKEN token _ _ _) =
  case token of
    VAR -> True
    CONST -> True
    _ -> False

isIdentifier :: Token -> Bool
isIdentifier (TOKEN token _ _ _) =
  case token of
    IDENTIFIER -> True
    _ -> False

isEqual :: Token -> Bool
isEqual (TOKEN token _ _ _) =
  case token of
    EQUAL -> True
    _ -> False
