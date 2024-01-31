module Scanner (scanTokens) where

import Data.Char (isDigit, isLetter, isSpace)
import Tokens

type SourceCode = [Char]

{- |
  The 'Pos' data type represents a position in the source code.

  'Pos' consists of two fields: 'line' and 'col'.
  - 'line' represents the line number in the source code, starting from 1.
  - 'col' represents the column number in a given line, also starting from 1.

  This 1-based numbering aligns with the conventional way of presenting line and column numbers
  in text editors, IDEs, and error messages, making it intuitive for users.

  Use 'Pos' to track the location of tokens, syntax elements, or errors in the source code
  during parsing, scanning, or interpreting.

  Example Usage:
  > Pos 3 15
  This represents the 15th column of the 3rd line in the source code.
-}
data Pos = Pos {line :: Int, col :: Int} deriving (Show, Eq)

incrLine :: Pos -> Pos
incrLine (Pos l _) = Pos (l + 1) 1

incrCol :: Pos -> Pos
incrCol (Pos l c) = Pos l (c + 1)

data LoxSyntaxError
  = SinglePosError String Pos
  | RangePosError String Pos Pos
  deriving (Eq)

instance Show LoxSyntaxError where
  show (SinglePosError msg pos) = msg ++ " at " ++ show pos
  show (RangePosError msg startPos endPos) = msg ++ " from " ++ show startPos ++ " to " ++ show endPos

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
  let (tokens, errors, pos) = scan str (Pos 1 1) [] []
   in case (tokens, errors) of
        ([], _) -> error $ "No tokens were actually scanned in " ++ show pos ++ " lines!"
        (_, _ : _) -> error $ show (length errors) ++ " errors were encountered: " ++ show errors
        (_, _) -> tokens

{- |
  'scan' - Recursive helper function for scanTokens that handles control flow and
  tracks the current parsing position within the string.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Pos' - Current line and col numbers.
    - '[Token]' - Accumulated list of tokens.
    - '[LoxSyntaxError]' - Accumulated list of error messages.

  Output:
    - '([Token], [LoxSyntaxError], Pos)' - Tuple containing the final list of tokens,
      errors encountered, and the final line and col numbers.
-}
scan :: SourceCode -> Pos -> [Token] -> [LoxSyntaxError] -> ([Token], [LoxSyntaxError], Pos)
scan [] pos tokens errors = (tokens, errors, pos)
scan current_string@(x : xs) pos@(Pos l _) tokensAcc errorsAcc
  | Just token <- tryBuildSimpleToken x pos =
      let newTokenAcc = tokensAcc ++ [token]
       in scan xs pos newTokenAcc errorsAcc
  | isOperatorToken x =
      if head xs == '='
        then
          let token = buildLongOperatorToken x pos
              newTokenAcc = tokensAcc ++ [token]
           in scan (tail xs) (incrCol pos) newTokenAcc errorsAcc
        else
          let token = buildShortOperatorToken x pos
              newTokenAcc = tokensAcc ++ [token]
           in scan xs (incrCol pos) newTokenAcc errorsAcc
  | x == '/' =
      if head xs == '/'
        then
          let newXs = swallowComment xs
           in scan newXs (incrLine pos) tokensAcc errorsAcc
        else
          let newTokensAcc = tokensAcc ++ [TOKEN SLASH "/" NONE l]
           in scan xs (incrCol pos) newTokensAcc errorsAcc
  | x == '"' =
      let (newXs, newPos, newTokensAcc, newErrorsAcc) = consumeString xs pos tokensAcc errorsAcc []
       in scan newXs newPos newTokensAcc newErrorsAcc
  | isDigit x =
      let (newXs, newPos, newTokensAcc, newErrorsAcc) = buildNumber current_string pos tokensAcc errorsAcc ""
       in scan newXs newPos newTokensAcc newErrorsAcc
  | isLetter x =
      let (newXs, newPos, newTokensAcc, newErrorsAcc) = buildWord current_string pos tokensAcc errorsAcc ""
       in scan newXs newPos newTokensAcc newErrorsAcc
  | x == '\n' = scan xs (incrLine pos) tokensAcc errorsAcc
  | isSpace x = scan xs (incrCol pos) tokensAcc errorsAcc
  | otherwise =
      let newErrorsAcc = errorsAcc ++ [SinglePosError ("Unexpected Char: " ++ [x]) pos]
       in scan xs (incrCol pos) tokensAcc newErrorsAcc

{- |
  'tryBuildSimpleToken' - Attempts to construct a simple token (like '*' or '+')
  based on the provided character.

  Input:
    - 'Char' - Character to evaluate.
    - 'pos' - Current line and column number.

  Output:
    - 'Maybe Token' - Just Token if a valid token is matched, Nothing otherwise.
-}
tryBuildSimpleToken :: Char -> Pos -> Maybe Token
tryBuildSimpleToken '(' (Pos l _) = Just $ TOKEN LEFT_PAREN "(" NONE l
tryBuildSimpleToken ')' (Pos l _) = Just $ TOKEN RIGHT_PAREN ")" NONE l
tryBuildSimpleToken '{' (Pos l _) = Just $ TOKEN LEFT_BRACE "{" NONE l
tryBuildSimpleToken '}' (Pos l _) = Just $ TOKEN RIGHT_BRACE "}" NONE l
tryBuildSimpleToken ',' (Pos l _) = Just $ TOKEN COMMA "," NONE l
tryBuildSimpleToken '.' (Pos l _) = Just $ TOKEN DOT "." NONE l
tryBuildSimpleToken '-' (Pos l _) = Just $ TOKEN MINUS "-" NONE l
tryBuildSimpleToken '+' (Pos l _) = Just $ TOKEN PLUS "+" NONE l
tryBuildSimpleToken ';' (Pos l _) = Just $ TOKEN SEMICOLON ";" NONE l
tryBuildSimpleToken '*' (Pos l _) = Just $ TOKEN STAR "*" NONE l
tryBuildSimpleToken _ _ = Nothing

isOperatorToken :: Char -> Bool
isOperatorToken c = c == '=' || c == '!' || c == '<' || c == '>'

buildShortOperatorToken :: Char -> Pos -> Token
buildShortOperatorToken '=' (Pos l _) = TOKEN EQUAL "=" NONE l
buildShortOperatorToken '!' (Pos l _) = TOKEN BANG "!" NONE l
buildShortOperatorToken '<' (Pos l _) = TOKEN LESS "<" NONE l
buildShortOperatorToken '>' (Pos l _) = TOKEN GREATER ">" NONE l
buildShortOperatorToken c (Pos l _) = error $ "Line: " ++ show l ++ ". Expected operator char but got: '" ++ [c] ++ "'"

buildLongOperatorToken :: Char -> Pos -> Token
buildLongOperatorToken '=' (Pos l _) = TOKEN EQUAL_EQUAL "==" NONE l
buildLongOperatorToken '!' (Pos l _) = TOKEN BANG_EQUAL "!=" NONE l
buildLongOperatorToken '<' (Pos l _) = TOKEN LESS_EQUAL "<=" NONE l
buildLongOperatorToken '>' (Pos l _) = TOKEN GREATER_EQUAL ">=" NONE l
buildLongOperatorToken c (Pos l _) = error $ "Line: " ++ show l ++ ". Expected operator char but got: '" ++ [c] ++ "'"

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
    - 'Pos - Current line and col number.
    - '[Token]' - Accumulated list of tokens.
    - '[LoxSyntaxError]' - Accumulated list of error messages.
    - 'String' - Accumulator for the string being constructed.

  Output:
    - '(SourceCode, Pos, [Token], [LoxSyntaxError ])' - Tuple containing the remaining
      code string, updated line and col number, accumulated tokens and accumalated errors.
-}
consumeString :: SourceCode -> Pos -> [Token] -> [LoxSyntaxError] -> String -> (SourceCode, Pos, [Token], [LoxSyntaxError])
consumeString [] pos tokensAcc errorsAcc stringAcc =
  let newError = SinglePosError ("String begin but does not end. String contains \"" ++ stringAcc ++ "\"") pos
   in ([], pos, tokensAcc, errorsAcc ++ [newError])
consumeString ('"' : xs) pos@(Pos l _) tokensAcc errorsAcc stringAcc =
  let newTokenAcc = TOKEN STRING ("\"" ++ stringAcc ++ "\"") (STR stringAcc) l
   in (xs, incrCol pos, tokensAcc ++ [newTokenAcc], errorsAcc)
consumeString ('\n' : xs) pos tokensAcc errorsAcc stringAcc =
  let newStringAcc = stringAcc ++ ['\n']
   in consumeString xs (incrLine pos) tokensAcc errorsAcc newStringAcc
consumeString (x : xs) pos tokensAcc errorsAcc stringAcc =
  let newStringAcc = stringAcc ++ [x]
   in consumeString xs (incrCol pos) tokensAcc errorsAcc newStringAcc

{- |
  'buildNumber' - Processes numeric literals from the code.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Pos' - Current line and col number.
    - '[Token]' - Accumulated list of tokens.
    - '[LoxSyntaxError]' - Accumulated list of error messages.
    - 'String' - Accumulator for the numeric literal being constructed.

  Output:
    - '(SourceCode, [Token], [LoxSyntaxError])' - Tuple containing the remaining
      code string, accumulated tokens and accumulated errors.
-}
buildNumber :: SourceCode -> Pos -> [Token] -> [LoxSyntaxError] -> String -> (SourceCode, Pos, [Token], [LoxSyntaxError])
buildNumber [] pos tokensAcc errorsAcc [] =
  let newError = SinglePosError "Empty digit" pos
   in ([], pos, tokensAcc, errorsAcc ++ [newError])
buildNumber [] pos tokensAcc errorsAcc numberAsString =
  let numberToken = buidNumberTokenFromString numberAsString pos
   in ([], pos, tokensAcc ++ [numberToken], errorsAcc)
buildNumber str@(x : xs) pos tokensAcc errorsAcc numberAsStringAcc
  | isDigit x =
      let newNumberAsStringAcc = numberAsStringAcc ++ [x]
       in buildNumber xs (incrCol pos) tokensAcc errorsAcc newNumberAsStringAcc
  | x == '.' =
      if '.' `elem` numberAsStringAcc || null xs || not (isDigit $ head xs)
        then
          let numberToken = buidNumberTokenFromString numberAsStringAcc pos
           in (str, pos, tokensAcc ++ [numberToken], errorsAcc)
        else
          let newNumberAsStringAcc = numberAsStringAcc ++ [x]
           in buildNumber xs (incrCol pos) tokensAcc errorsAcc newNumberAsStringAcc
  | otherwise =
      let numberToken = buidNumberTokenFromString numberAsStringAcc pos
       in (str, pos, tokensAcc ++ [numberToken], errorsAcc)

buidNumberTokenFromString :: String -> Pos -> Token
buidNumberTokenFromString numberAsString (Pos l _) =
  let number = read numberAsString :: Float
   in TOKEN NUMBER numberAsString (NUM number) l

{- |
  'buildWord' - Processes identifiers/keywords from the code.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Pos' - Current line and col number.
    - '[Token]' - Accumulated list of tokens.
    - '[LoxSyntaxError]' - Accumulated list of error messages.
    - 'String' - Accumulator for the identifier/keyword being constructed.

  Output:
    - '(SourceCode, [Token], [LoxSyntaxError])' - Tuple containing the remaining
      code string, accumulated tokens and accumalated errors.
-}
buildWord :: SourceCode -> Pos -> [Token] -> [LoxSyntaxError] -> String -> (SourceCode, Pos, [Token], [LoxSyntaxError])
buildWord [] pos tokensAcc errorsAcc [] =
  let newError = SinglePosError "Empty word" pos
   in ([], pos, tokensAcc, errorsAcc ++ [newError])
buildWord [] pos tokensAcc errorsAcc numberAsString =
  let wordToken = buildWordFromString numberAsString pos
   in ([], pos, tokensAcc ++ [wordToken], errorsAcc)
buildWord str@(x : xs) pos tokensAcc errorsAcc wordAcc
  | isLoxAlphaNumerical x = buildWord xs (incrCol pos) tokensAcc errorsAcc (wordAcc ++ [x])
  | otherwise =
      let wordToken = buildWordFromString wordAcc pos
       in (str, pos, tokensAcc ++ [wordToken], errorsAcc)

buildWordFromString :: String -> Pos -> Token
buildWordFromString "and" (Pos l _) = TOKEN AND "and" NONE l
buildWordFromString "class" (Pos l _) = TOKEN CLASS "class" NONE l
buildWordFromString "else" (Pos l _) = TOKEN ELSE "else" NONE l
buildWordFromString "false" (Pos l _) = TOKEN FALSE "false" FALSE_LIT l
buildWordFromString "for" (Pos l _) = TOKEN FOR "for" NONE l
buildWordFromString "fun" (Pos l _) = TOKEN FUN "fun" NONE l
buildWordFromString "if" (Pos l _) = TOKEN IF "if" NONE l
buildWordFromString "nil" (Pos l _) = TOKEN NIL "nil" NIL_LIT l
buildWordFromString "or" (Pos l _) = TOKEN OR "or" NONE l
buildWordFromString "print" (Pos l _) = TOKEN PRINT "print" NONE l
buildWordFromString "return" (Pos l _) = TOKEN RETURN "return" NONE l
buildWordFromString "super" (Pos l _) = TOKEN SUPER "super" NONE l
buildWordFromString "this" (Pos l _) = TOKEN THIS "this" NONE l
buildWordFromString "true" (Pos l _) = TOKEN TRUE "true" TRUE_LIT l
buildWordFromString "var" (Pos l _) = TOKEN VAR "var" NONE l
buildWordFromString "while" (Pos l _) = TOKEN WHILE "while" NONE l
buildWordFromString identifier (Pos l _) = TOKEN IDENTIFIER identifier (ID identifier) l

isLoxLetter :: Char -> Bool
isLoxLetter c = isLetter c || c == '_'

isLoxAlphaNumerical :: Char -> Bool
isLoxAlphaNumerical c = isLoxLetter c || isDigit c
