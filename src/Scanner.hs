module Scanner (scanTokens) where

import Data.Char (isDigit, isLetter, isSpace)
import Tokens

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
scanTokens sourceCode =
  let (tokens, errors, Pos l _) = scan sourceCode (Pos 1 1) [] []
   in case (tokens, errors) of
        (_, _ : _) -> error . unlines $ formatErrors errors sourceCode
        ([], _) -> error "No tokens were actually scanned."
        (_, _) -> tokens ++ [TOKEN EOF "" NONE l]

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
data Pos = Pos Int Int deriving (Eq)

instance Show Pos where
  show (Pos l c) = show l ++ ":" ++ show c

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

type SourceCode = [Char]

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
scan currentCode@(x : xs) pos@(Pos l _) tokensAcc errorsAcc
  | Just token <- tryBuildSimpleToken x pos =
      let newTokenAcc = tokensAcc ++ [token]
       in scan xs pos newTokenAcc errorsAcc
  | isOperatorToken x =
      if not (null xs) && head xs == '='
        then
          let token = buildLongOperatorToken x pos
              newTokenAcc = tokensAcc ++ [token]
           in scan (tail xs) (incrCol pos) newTokenAcc errorsAcc
        else
          let token = buildShortOperatorToken x pos
              newTokenAcc = tokensAcc ++ [token]
           in scan xs (incrCol pos) newTokenAcc errorsAcc
  | x == '/' =
      if not (null xs) && head xs == '/'
        then
          let newXs = swallowComment xs
           in scan newXs (incrLine pos) tokensAcc errorsAcc
        else
          let newTokensAcc = tokensAcc ++ [TOKEN SLASH "/" NONE l]
           in scan xs (incrCol pos) newTokensAcc errorsAcc
  | x == '"' =
      let (newXs, newPos, result) = buildString xs pos
       in case result of
            Left newToken -> scan newXs newPos (tokensAcc ++ [newToken]) errorsAcc
            Right newError -> scan newXs newPos tokensAcc (errorsAcc ++ [newError])
  | isDigit x =
      let (newXs, newPos, result) = buildNumber currentCode pos
       in case result of
            Left newToken -> scan newXs newPos (tokensAcc ++ [newToken]) errorsAcc
            Right newError -> scan newXs newPos tokensAcc (errorsAcc ++ [newError])
  | isLoxLetter x =
      let (newXs, newPos, result) = buildWord currentCode pos
       in case result of
            Left newToken -> scan newXs newPos (tokensAcc ++ [newToken]) errorsAcc
            Right newError -> scan newXs newPos tokensAcc (errorsAcc ++ [newError])
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
  'buildString' - Parses a string literal from the code.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Pos - Current line and col number.

  Output:
    - '(SourceCode, Pos, Either Token LoxSyntaxError)' - Tuple containing the remaining
      code string, updated line and col number, and a result of either a scanned token
      or an error.
-}
buildString :: SourceCode -> Pos -> (SourceCode, Pos, Either Token LoxSyntaxError)
buildString sourceCode originalPos@(Pos startingLine _) = buildStringHelper sourceCode originalPos ""
 where
  buildStringHelper [] pos stringAcc =
    let newError = RangePosError ("String begin but does not end. String contains \"" ++ stringAcc ++ "\"") originalPos pos
     in ([], pos, Right newError)
  buildStringHelper ('"' : xs) pos stringAcc =
    let newTokenAcc = TOKEN STRING ("\"" ++ stringAcc ++ "\"") (STR stringAcc) startingLine
     in (xs, incrCol pos, Left newTokenAcc)
  buildStringHelper ('\n' : xs) pos stringAcc =
    let newStringAcc = stringAcc ++ ['\n']
     in buildStringHelper xs (incrLine pos) newStringAcc
  buildStringHelper (x : xs) pos stringAcc =
    let newStringAcc = stringAcc ++ [x]
     in buildStringHelper xs (incrCol pos) newStringAcc

{- |
  'buildNumber' - Processes numeric literals from the code.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Pos' - Current line and col number.

  Output:
    - '(SourceCode, Pos, Either Token LoxSyntaxError)' - Tuple containing the remaining
      code string, updated line and col number, and a result of either a scanned token
      or an error.
-}
buildNumber :: SourceCode -> Pos -> (SourceCode, Pos, Either Token LoxSyntaxError)
buildNumber sourceCode originalPos = buildNumberHelper sourceCode originalPos []
 where
  buildNumberHelper [] pos [] =
    let newError = SinglePosError "Empty digit" pos
     in ([], pos, Right newError)
  buildNumberHelper [] pos numberAsString =
    let numberToken = buidNumberTokenFromString numberAsString pos
     in ([], pos, Left numberToken)
  buildNumberHelper str@(x : xs) pos numberAsStringAcc
    | isDigit x =
        let newNumberAsStringAcc = numberAsStringAcc ++ [x]
         in buildNumberHelper xs (incrCol pos) newNumberAsStringAcc
    | x == '.' =
        if '.' `elem` numberAsStringAcc || null xs || not (isDigit $ head xs)
          then
            let numberToken = buidNumberTokenFromString numberAsStringAcc pos
             in (str, pos, Left numberToken)
          else
            let newNumberAsStringAcc = numberAsStringAcc ++ [x]
             in buildNumberHelper xs (incrCol pos) newNumberAsStringAcc
    | otherwise =
        let numberToken = buidNumberTokenFromString numberAsStringAcc pos
         in (str, pos, Left numberToken)

buidNumberTokenFromString :: String -> Pos -> Token
buidNumberTokenFromString numberAsString (Pos l _) =
  let number = read numberAsString :: Float
   in TOKEN NUMBER numberAsString (NUM number) l

{- |
  'buildWord' - Processes identifiers/keywords from the code.

  Input:
    - 'SourceCode' - Current part of the code string being processed.
    - 'Pos' - Current line and col number.

  Output:
    - '(SourceCode, Pos, Either Token LoxSyntaxError)' - Tuple containing the remaining
      code string, updated line and col number, and a result of either a scanned token
      or an error.
-}
buildWord :: SourceCode -> Pos -> (SourceCode, Pos, Either Token LoxSyntaxError)
buildWord sourceCode originalPos = buildWordHelper sourceCode originalPos []
 where
  buildWordHelper [] pos [] =
    let newError = SinglePosError "Empty word" pos
     in ([], pos, Right newError)
  buildWordHelper [] pos numberAsString =
    let wordToken = buildWordFromString numberAsString pos
     in ([], pos, Left wordToken)
  buildWordHelper str@(x : xs) pos wordAcc
    | isLoxAlphaNumerical x = buildWordHelper xs (incrCol pos) (wordAcc ++ [x])
    | otherwise =
        let wordToken = buildWordFromString wordAcc pos
         in (str, pos, Left wordToken)

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

{- |
  'formatErrors' processes a list of 'LoxSyntaxError's and formats them for display.

  This function takes a list of syntax errors encountered during the scanning or parsing
  of Lox source code, and formats each error with context from the source code. It utilizes
  'showErrorInCode' to generate a user-friendly string representation for each error,
  including line numbers and visual indicators of error locations.

  Input:
    - 'loxSyntaxErrors': A list of errors encountered during scanning or parsing.
    - 'sourceCode': The source code being scanned or parsed, as a string.

  Output:
    - A list of strings, where each string is a formatted error message with contextual information
      from the source code. Errors are separated by newlines for clear readability.
-}
formatErrors :: [LoxSyntaxError] -> SourceCode -> [String]
formatErrors loxSyntaxErrors sourceCode = go loxSyntaxErrors []
 where
  go [] errorsAsStrings = errorsAsStrings
  go (e : ers) errAcc =
    let sourceCodeAsLines = lines sourceCode
        errorInCode = showErrorInCode sourceCodeAsLines e
        newErrAcc = errAcc ++ ['\n' : show e] ++ errorInCode
     in go ers newErrAcc

{- |
'ShowErrorInCode' displays an error message in context with the source code.

Input:
  - '[SourceCode]': The source code being scanned or parsed, as list of string where every line is one element in the
  -                 list. E.g. "var x\n@" => ["var x", "@"]
  - 'LoxSyntaxError': An error type which can be either a 'SinglePosError' indicating an error at a single position,
    or a 'RangePosError' indicating an error spanning a range from a start to an end position.

Output:
  - A list of strings ('[String]') formatted for display, showing the error in context with its location in the source code.
-}
showErrorInCode :: [SourceCode] -> LoxSyntaxError -> [String]
showErrorInCode sourceCodeAsLines (SinglePosError _ (Pos l c)) =
  [ show l ++ " " ++ sourceCodeAsLines !! (l - 1)
  , "  " ++ replicate (c - 1) ' ' ++ "^"
  ]
showErrorInCode sourceCodeAsLines (RangePosError _ (Pos l1 c1) (Pos l2 c2))
  | l1 == l2 =
      [ show l1 ++ " " ++ sourceCodeAsLines !! (l1 - 1)
      , "  " ++ replicate (c1 - 1) ' ' ++ replicate (c2 - c1 + 1) '^'
      ]
  | otherwise =
      let startLine = sourceCodeAsLines !! (l1 - 1)
          endLine = if l2 - 1 < length sourceCodeAsLines then sourceCodeAsLines !! (l2 - 1) else " "
          startLineIndicator = show l1 ++ " " ++ startLine
          endLineIndicator = show l2 ++ " " ++ endLine
          midLines = map (\l -> show l ++ " " ++ (sourceCodeAsLines !! (l - 1))) [l1 + 1 .. l2 - 1]
          startLineCaret = replicate (length (show l1) + 1) ' ' ++ replicate (c1 - 1) ' ' ++ "^"
          endLineCaret = replicate (length (show l2) + 1) ' ' ++ replicate (c2 - 1) ' ' ++ "^"
       in [startLineIndicator, startLineCaret] ++ midLines ++ [endLineIndicator, endLineCaret]
