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
    | Just token <- tryBuildSimpleToken x line =
        let newTokenAcc = tokensAcc ++ [token]
         in go xs line newTokenAcc errorsAcc
    | isOperatorToken x =
        if head xs == '='
          then
            let token = buildLongOperatorToken x line
                newTokenAcc = tokensAcc ++ [token]
             in go (tail xs) line newTokenAcc errorsAcc
          else
            let token = buildShortOperatorToken x line
                newTokenAcc = tokensAcc ++ [token]
             in go xs line newTokenAcc errorsAcc
    | x == '/' =
        if head xs == '/'
          then
            let newXs = swallowComment xs
             in go newXs (line + 1) tokensAcc errorsAcc
          else
            let newTokensAcc = tokensAcc ++ [TOKEN SLASH "/" NONE line]
             in go xs line newTokensAcc errorsAcc
    | x == '"' =
        let (newXs, newLine, newTokensAcc, newErrorsAcc) = consumeString xs line tokensAcc errorsAcc []
         in go newXs newLine newTokensAcc newErrorsAcc
    | x == '\n' = go xs (line + 1) tokensAcc errorsAcc
    | isSpace x = go xs line tokensAcc errorsAcc
    | otherwise =
        let newErrorsAcc = errorsAcc ++ ["Invalid character: '" ++ [x] ++ "' at line " ++ show line]
         in go xs line tokensAcc newErrorsAcc

tryBuildSimpleToken :: Char -> Int -> Maybe Token
tryBuildSimpleToken '(' line = Just $ TOKEN LEFT_PAREN "(" NONE line
tryBuildSimpleToken ')' line = Just $ TOKEN RIGHT_PAREN ")" NONE line
tryBuildSimpleToken '{' line = Just $ TOKEN LEFT_BRACE "{" NONE line
tryBuildSimpleToken '}' line = Just $ TOKEN RIGHT_BRACE "}" NONE line
tryBuildSimpleToken ',' line = Just $ TOKEN COMMA "," NONE line
tryBuildSimpleToken '.' line = Just $ TOKEN DOT "." NONE line
tryBuildSimpleToken '-' line = Just $ TOKEN MINUS "-" NONE line
tryBuildSimpleToken '+' line = Just $ TOKEN PLUS "+" NONE line
tryBuildSimpleToken ';' line = Just $ TOKEN SEMICOLON ";" NONE line
tryBuildSimpleToken '*' line = Just $ TOKEN STAR "*" NONE line
tryBuildSimpleToken _ _ = Nothing

isOperatorToken :: Char -> Bool
isOperatorToken c = c == '=' || c == '!' || c == '<' || c == '>'

buildShortOperatorToken :: Char -> Int -> Token
buildShortOperatorToken '=' line = TOKEN EQUAL "=" NONE line
buildShortOperatorToken '!' line = TOKEN BANG "!" NONE line
buildShortOperatorToken '<' line = TOKEN LESS "<" NONE line
buildShortOperatorToken '>' line = TOKEN GREATER ">" NONE line
buildShortOperatorToken c line = error $ "Line: " ++ show line ++ ". Expected operator char but got: '" ++ [c] ++ "'"

buildLongOperatorToken :: Char -> Int -> Token
buildLongOperatorToken '=' line = TOKEN EQUAL_EQUAL "==" NONE line
buildLongOperatorToken '!' line = TOKEN BANG_EQUAL "!=" NONE line
buildLongOperatorToken '<' line = TOKEN LESS_EQUAL "<=" NONE line
buildLongOperatorToken '>' line = TOKEN GREATER_EQUAL ">=" NONE line
buildLongOperatorToken c line = error $ "Line: " ++ show line ++ ". Expected operator char but got: '" ++ [c] ++ "'"

swallowComment :: String -> String
swallowComment [] = []
swallowComment ('\n' : xs) = xs
swallowComment (_ : xs) = swallowComment xs

consumeString :: String -> Int -> [Token] -> [String] -> String -> (String, Int, [Token], [String])
consumeString [] line tokensAcc errorsAcc stringAcc =
  let newError = "String begin but does not end. String contains \"" ++ stringAcc ++ "\"" :: String
   in ([], line, tokensAcc, errorsAcc ++ [newError])
consumeString ('"' : xs) line tokensAcc errorsAcc stringAcc =
  let newTokenAcc = TOKEN STRING ("\"" ++ stringAcc ++ "\"") (STR stringAcc) line
   in (xs, line, tokensAcc ++ [newTokenAcc], errorsAcc)
consumeString ('\n' : xs) line tokensAcc errorsAcc stringAcc =
  let newStringAcc = stringAcc ++ ['\n']
      newLine = line + 1
   in consumeString xs newLine tokensAcc errorsAcc newStringAcc
consumeString (x : xs) line tokensAcc errorsAcc stringAcc =
  let newStringAcc = stringAcc ++ [x]
   in consumeString xs line tokensAcc errorsAcc newStringAcc

-- consumeString [] line tokensAcc errorsAcc str =
--   let newStringToken = TOKEN String str
