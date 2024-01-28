module Scanner (scanTokens) where

import Data.Char (isDigit, isLetter, isSpace)
import Tokens

scanTokens :: [Char] -> [Token]
scanTokens str = go str 1 [] []
 where
  go [] _ tokens errors
    | not (null errors) = error $ "Encountered errors while scanning: " ++ show errors
    | otherwise = tokens
  go current_string@(x : xs) line tokensAcc errorsAcc
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
    | isDigit x =
        let (newXs, newTokensAcc, newErrorsAcc) = buildNumber current_string line tokensAcc errorsAcc ""
         in go newXs line newTokensAcc newErrorsAcc
    | isLetter x =
        let (newXs, newTokensAcc, newErrorsAcc) = buildWord current_string line tokensAcc errorsAcc ""
         in go newXs line newTokensAcc newErrorsAcc
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

buildNumber :: String -> Int -> [Token] -> [String] -> String -> (String, [Token], [String])
buildNumber [] line tokensAcc errorsAcc [] =
  let newError = "Empty digit on line " ++ show line
   in ([], tokensAcc, errorsAcc ++ [newError])
buildNumber [] line tokensAcc errorsAcc numberAsString =
  let numberToken = buidNumberTokenFromString numberAsString line
   in ([], tokensAcc ++ [numberToken], errorsAcc)
buildNumber str@(x : xs) line tokensAcc errorsAcc numberAsStringAcc
  | isDigit x =
      let newNumberAsStringAcc = numberAsStringAcc ++ [x]
       in buildNumber xs line tokensAcc errorsAcc newNumberAsStringAcc
  | x == '.' =
      if '.' `elem` numberAsStringAcc || null xs || not (isDigit $ head xs)
        then
          let numberToken = buidNumberTokenFromString numberAsStringAcc line
           in (str, tokensAcc ++ [numberToken], errorsAcc)
        else
          let newNumberAsStringAcc = numberAsStringAcc ++ [x]
           in buildNumber xs line tokensAcc errorsAcc newNumberAsStringAcc
  | otherwise =
      let numberToken = buidNumberTokenFromString numberAsStringAcc line
       in (str, tokensAcc ++ [numberToken], errorsAcc)

buidNumberTokenFromString :: String -> Int -> Token
buidNumberTokenFromString numberAsString line =
  let number = read numberAsString :: Float
   in TOKEN NUMBER numberAsString (NUM number) line

buildWord :: String -> Int -> [Token] -> [String] -> String -> (String, [Token], [String])
buildWord [] line tokensAcc errorsAcc [] =
  let newError = "Empty word on line " ++ show line
   in ([], tokensAcc, errorsAcc ++ [newError])
buildWord [] line tokensAcc errorsAcc numberAsString =
  let wordToken = buildWordFromString numberAsString line
   in ([], tokensAcc ++ [wordToken], errorsAcc)
buildWord str@(x : xs) line tokensAcc errorsAcc wordAcc
  | isLoxAlphaNumerical x = buildWord xs line tokensAcc errorsAcc (wordAcc ++ [x])
  | otherwise =
      let wordToken = buildWordFromString wordAcc line
       in (str, tokensAcc ++ [wordToken], errorsAcc)

buildWordFromString :: String -> Int -> Token
buildWordFromString "and" line = TOKEN AND "and" NONE line
buildWordFromString "class" line = TOKEN CLASS "class" NONE line
buildWordFromString "else" line = TOKEN ELSE "else" NONE line
buildWordFromString "false" line = TOKEN FALSE "false" FALSE_LIT line
buildWordFromString "for" line = TOKEN FOR "for" NONE line
buildWordFromString "fun" line = TOKEN FUN "fun" NONE line
buildWordFromString "if" line = TOKEN IF "if" NONE line
buildWordFromString "nil" line = TOKEN NIL "nil" NIL_LIT line
buildWordFromString "or" line = TOKEN OR "or" NONE line
buildWordFromString "print" line = TOKEN PRINT "print" NONE line
buildWordFromString "return" line = TOKEN RETURN "return" NONE line
buildWordFromString "super" line = TOKEN SUPER "super" NONE line
buildWordFromString "this" line = TOKEN THIS "this" NONE line
buildWordFromString "true" line = TOKEN TRUE "true" TRUE_LIT line
buildWordFromString "var" line = TOKEN VAR "var" NONE line
buildWordFromString "while" line = TOKEN WHILE "while" NONE line
buildWordFromString identifier line = TOKEN IDENTIFIER identifier (ID identifier) line

isLoxLetter :: Char -> Bool
isLoxLetter c = isLetter c || c == '_'

isLoxAlphaNumerical :: Char -> Bool
isLoxAlphaNumerical c = isLoxLetter c || isDigit c
