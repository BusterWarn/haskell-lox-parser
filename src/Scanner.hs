module Scanner (scanTokens) where

import Data.Char (isDigit, isLetter, isSpace)
import Tokens

type SourceCode = [Char]
type LoxSyntaxError = String
type Line = Int

{- |
  'scanTokens' - Main function that scans a string of code and generates tokens.
  Throws errors for any encountered scanning issues.

  Input:
    - '[Char]' - A string representing the code to be scanned.

  Output:
    - '[Token]' - A list of generated tokens.
-}
scanTokens :: [Char] -> [Token]
scanTokens [] = error "Empty input string to scanner"
scanTokens str =
  let (tokens, errors, line) = scan str 1 [] []
   in case (tokens, errors) of
        ([], _) -> error $ "No tokens were actually scanned in " ++ show line ++ " lines!"
        (_, _ : _) -> error $ show (length errors) ++ " errors were encountered: " ++ show errors
        (_, _) -> tokens

{- |
  'scan' - Recursive helper function for scanTokens that handles control flow and
  tracks the current parsing position within the string.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Line' - Current line number.
    - '[Token]' - Accumulated list of tokens.
    - '[LoxSyntaxError]' - Accumulated list of error messages.

  Output:
    - '([Token], [LoxSyntaxError], Line)' - Tuple containing the final list of tokens,
      errors encountered, and the final line number.
-}
scan :: SourceCode -> Line -> [Token] -> [LoxSyntaxError] -> ([Token], [LoxSyntaxError], Line)
scan [] line tokens errors = (tokens, errors, line)
scan current_string@(x : xs) line tokensAcc errorsAcc
  | Just token <- tryBuildSimpleToken x line =
      let newTokenAcc = tokensAcc ++ [token]
       in scan xs line newTokenAcc errorsAcc
  | isOperatorToken x =
      if head xs == '='
        then
          let token = buildLongOperatorToken x line
              newTokenAcc = tokensAcc ++ [token]
           in scan (tail xs) line newTokenAcc errorsAcc
        else
          let token = buildShortOperatorToken x line
              newTokenAcc = tokensAcc ++ [token]
           in scan xs line newTokenAcc errorsAcc
  | x == '/' =
      if head xs == '/'
        then
          let newXs = swallowComment xs
           in scan newXs (line + 1) tokensAcc errorsAcc
        else
          let newTokensAcc = tokensAcc ++ [TOKEN SLASH "/" NONE line]
           in scan xs line newTokensAcc errorsAcc
  | x == '"' =
      let (newXs, newLine, newTokensAcc, newErrorsAcc) = consumeString xs line tokensAcc errorsAcc []
       in scan newXs newLine newTokensAcc newErrorsAcc
  | isDigit x =
      let (newXs, newTokensAcc, newErrorsAcc) = buildNumber current_string line tokensAcc errorsAcc ""
       in scan newXs line newTokensAcc newErrorsAcc
  | isLetter x =
      let (newXs, newTokensAcc, newErrorsAcc) = buildWord current_string line tokensAcc errorsAcc ""
       in scan newXs line newTokensAcc newErrorsAcc
  | x == '\n' = scan xs (line + 1) tokensAcc errorsAcc
  | isSpace x = scan xs line tokensAcc errorsAcc
  | otherwise =
      let newErrorsAcc = errorsAcc ++ ["Invalid character: '" ++ [x] ++ "' at line " ++ show line]
       in scan xs line tokensAcc newErrorsAcc

{- |
  'tryBuildSimpleToken' - Attempts to construct a simple token (like '*' or '+')
  based on the provided character.

  Input:
    - 'Char' - Character to evaluate.
    - 'Line' - Current line number.

  Output:
    - 'Maybe Token' - Just Token if a valid token is matched, Nothing otherwise.
-}
tryBuildSimpleToken :: Char -> Line -> Maybe Token
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

buildShortOperatorToken :: Char -> Line -> Token
buildShortOperatorToken '=' line = TOKEN EQUAL "=" NONE line
buildShortOperatorToken '!' line = TOKEN BANG "!" NONE line
buildShortOperatorToken '<' line = TOKEN LESS "<" NONE line
buildShortOperatorToken '>' line = TOKEN GREATER ">" NONE line
buildShortOperatorToken c line = error $ "Line: " ++ show line ++ ". Expected operator char but got: '" ++ [c] ++ "'"

buildLongOperatorToken :: Char -> Line -> Token
buildLongOperatorToken '=' line = TOKEN EQUAL_EQUAL "==" NONE line
buildLongOperatorToken '!' line = TOKEN BANG_EQUAL "!=" NONE line
buildLongOperatorToken '<' line = TOKEN LESS_EQUAL "<=" NONE line
buildLongOperatorToken '>' line = TOKEN GREATER_EQUAL ">=" NONE line
buildLongOperatorToken c line = error $ "Line: " ++ show line ++ ". Expected operator char but got: '" ++ [c] ++ "'"

{- |
  'swallowComment' - Processes a comment part of the code and finds the position
  where the code resumes after the comment.

  Input:
    - 'SourceCode' - The part of the code string starting with a comment.

  Output:
    - 'SourceCode' - Remaining code string after the comment.
-}
swallowComment :: SourceCode -> SourceCode
swallowComment [] = []
swallowComment ('\n' : xs) = xs
swallowComment (_ : xs) = swallowComment xs

{- |
  'consumeString' - Parses a string literal from the code.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Line' - Current line number.
    - '[Token]' - Accumulated list of tokens.
    - '[LoxSyntaxError]' - Accumulated list of error messages.
    - 'String' - Accumulator for the string being constructed.

  Output:
    - '(SourceCode, Line, [Token], [LoxSyntaxError ])' - Tuple containing the remaining
      code string, updated line number, accumulated tokens and accumalated errors.
-}
consumeString :: SourceCode -> Line -> [Token] -> [LoxSyntaxError] -> String -> (SourceCode, Line, [Token], [LoxSyntaxError])
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

{- |
  'buildNumber' - Processes numeric literals from the code.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Line' - Current line number.
    - '[Token]' - Accumulated list of tokens.
    - '[LoxSyntaxError]' - Accumulated list of error messages.
    - 'String' - Accumulator for the numeric literal being constructed.

  Output:
    - '(SourceCode, [Token], [LoxSyntaxError])' - Tuple containing the remaining
      code string, accumulated tokens and accumulated errors.
-}
buildNumber :: SourceCode -> Line -> [Token] -> [LoxSyntaxError] -> String -> (SourceCode, [Token], [LoxSyntaxError])
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

buidNumberTokenFromString :: String -> Line -> Token
buidNumberTokenFromString numberAsString line =
  let number = read numberAsString :: Float
   in TOKEN NUMBER numberAsString (NUM number) line

{- |
  'buildWord' - Processes identifiers/keywords from the code.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Line' - Current line number.
    - '[Token]' - Accumulated list of tokens.
    - '[LoxSyntaxError]' - Accumulated list of error messages.
    - 'String' - Accumulator for the identifier/keyword being constructed.

  Output:
    - '(SourceCode, [Token], [LoxSyntaxError])' - Tuple containing the remaining
      code string, accumulated tokens and accumalated errors.
-}
buildWord :: SourceCode -> Line -> [Token] -> [LoxSyntaxError] -> String -> (SourceCode, [Token], [LoxSyntaxError])
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

buildWordFromString :: String -> Line -> Token
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
