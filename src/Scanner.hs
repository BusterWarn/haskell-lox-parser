module Scanner (scanTokens) where

import Data.Char (isSpace)
import Tokens

scanTokens :: [Char] -> [Token]
scanTokens str = go str 1 [] []
 where
  go [] _ tokens errors
    | not (null errors) = error $ "Encountered errors while scanning: " ++ show errors
    | otherwise = tokens
  go (x : xs) line tokensAcc errorsAcc
    | Just token <- simpleCharToToken x line =
        let newTokenAcc = tokensAcc ++ [token]
         in go xs line newTokenAcc errorsAcc
    | x == '\n' = go xs (line + 1) tokensAcc errorsAcc
    | isSpace x = go xs line tokensAcc errorsAcc
    | otherwise =
        let newErrorsAcc = errorsAcc ++ ["Invalid character: '" ++ [x] ++ "' at line " ++ show line]
         in go xs line tokensAcc newErrorsAcc

simpleCharToToken :: Char -> Int -> Maybe Token
simpleCharToToken '(' line = Just $ TOKEN LEFT_PAREN "(" NONE line
simpleCharToToken ')' line = Just $ TOKEN RIGHT_PAREN ")" NONE line
simpleCharToToken '{' line = Just $ TOKEN LEFT_BRACE "{" NONE line
simpleCharToToken '}' line = Just $ TOKEN RIGHT_BRACE "}" NONE line
simpleCharToToken ',' line = Just $ TOKEN COMMA "," NONE line
simpleCharToToken '.' line = Just $ TOKEN DOT "." NONE line
simpleCharToToken '-' line = Just $ TOKEN MINUS "-" NONE line
simpleCharToToken '+' line = Just $ TOKEN PLUS "+" NONE line
simpleCharToToken ';' line = Just $ TOKEN SEMICOLON ";" NONE line
simpleCharToToken '*' line = Just $ TOKEN STAR "*" NONE line
simpleCharToToken _ _ = Nothing
