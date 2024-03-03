import qualified Parser
import qualified Scanner
import qualified Tokens

import Control.Exception (evaluate)
import Data.Char (isSpace)
import Test.Hspec
import Tokens (TokenType (AND, BANG, BANG_EQUAL, CLASS, COMMA, DOT, ELSE, EOF, EQUAL, EQUAL_EQUAL, FALSE, FOR, FUN, GREATER, GREATER_EQUAL, IDENTIFIER, IF, LEFT_BRACE, LEFT_PAREN, LESS, LESS_EQUAL, MINUS, NIL, NUMBER, OR, PLUS, PRINT, RETURN, RIGHT_BRACE, RIGHT_PAREN, SEMICOLON, SLASH, STAR, STRING, SUPER, THIS, TRUE, VAR, WHILE))

main :: IO ()
main = hspec tests

tokenToTokenType :: Tokens.Token -> Tokens.TokenType
tokenToTokenType (Tokens.TOKEN token _ _ _) = token

tokenToLineNumber :: Tokens.Token -> Int
tokenToLineNumber (Tokens.TOKEN _ _ _ line) = line

-- Removes all whitespace from a string, including newlines
removeWhitespace :: String -> String
removeWhitespace = filter (not . isSpace)

-- Custom infix function for testing
shouldParseAs :: String -> String -> Expectation
input `shouldParseAs` expected =
  let parsedStatements = Parser.parse $ Scanner.scanTokens input
      actualShowString = show parsedStatements
      actualNoWhitespace = removeWhitespace actualShowString
      expectedNoWhitespace = removeWhitespace expected
   in actualNoWhitespace `shouldBe` expectedNoWhitespace

tests :: Spec
tests = do
  describe "Scan correct token types" $ do
    it "Scans all simple characters" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "(){},.-+;*"
      result `shouldBe` [LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, STAR, EOF]
    it "Scans all simple characters and ignores whitespace" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "(){}\n,. -+\t;   *\r"
      result `shouldBe` [LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE, COMMA, DOT, MINUS, PLUS, SEMICOLON, STAR, EOF]
    it "Throws an error for invalid characters" $ do
      evaluate (Scanner.scanTokens "{@}") `shouldThrow` anyException
    it "Scans operator tokens" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "= ! < > == === != <= >="
      result `shouldBe` [EQUAL, BANG, LESS, GREATER, EQUAL_EQUAL, EQUAL_EQUAL, EQUAL, BANG_EQUAL, LESS_EQUAL, GREATER_EQUAL, EOF]
    it "Can scan = as last token" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "==="
      result `shouldBe` [EQUAL_EQUAL, EQUAL, EOF]
    it "Can scan / as last token" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "/"
      result `shouldBe` [SLASH, EOF]
    it "Does not scan tokens" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "/+/ // / comment = a * \n//\n ** ==//"
      result `shouldBe` [SLASH, PLUS, SLASH, STAR, STAR, EQUAL_EQUAL, EOF]
    it "Can scan strings" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "+ \" Hi \n Mom! //\" =="
      result `shouldBe` [PLUS, STRING, EQUAL_EQUAL, EOF]
    it "Throws an error if string does not end" $ do
      evaluate (Scanner.scanTokens "+ \" Hi \n Mom! // ==") `shouldThrow` anyException
    it "Can scan simple int" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "1"
      result `shouldBe` [NUMBER, EOF]
    it "Can scan simple float" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "1.0"
      result `shouldBe` [NUMBER, EOF]
    it "Can scan an int correctly" $ do
      let result = head $ Scanner.scanTokens "999"
      case result of
        Tokens.TOKEN _ str (Tokens.NUM value) _ -> do
          str `shouldBe` "999"
          value `shouldBe` 999.0
        _ -> error "Expected an identifier token"
    it "Can scan a float correctly" $ do
      let result = head $ Scanner.scanTokens "999.999"
      case result of
        Tokens.TOKEN _ str (Tokens.NUM value) _ -> do
          str `shouldBe` "999.999"
          value `shouldBe` 999.999
        _ -> error "Expected an identifier token"
    it "Can scan a complicated float correctly" $ do
      let result = head $ Scanner.scanTokens "0000010999.999"
      case result of
        Tokens.TOKEN _ str (Tokens.NUM value) _ -> do
          str `shouldBe` "0000010999.999"
          value `shouldBe` 10999.999
        _ -> error "Expected an identifier token"
    it "Can scan numbers" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "1 .1 1. 1.1 1.1.\n123456789 12345.6789"
      result `shouldBe` [NUMBER, DOT, NUMBER, NUMBER, DOT, NUMBER, NUMBER, DOT, NUMBER, NUMBER, EOF]
    it "Can scan words and identifiers" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "hi var m0m_ 1I loVe Y0U. for and true _ w1238ukjdsd_anjkdsf_ _123"
      result `shouldBe` [IDENTIFIER, VAR, IDENTIFIER, NUMBER, IDENTIFIER, IDENTIFIER, IDENTIFIER, DOT, FOR, AND, TRUE, IDENTIFIER, IDENTIFIER, IDENTIFIER, EOF]
    it "Can scan an identifier correctly" $ do
      let result = head $ Scanner.scanTokens "\n\n_I_am_your_f4THER"
      case result of
        Tokens.TOKEN _ str (Tokens.ID idStr) line -> do
          str `shouldBe` "_I_am_your_f4THER"
          idStr `shouldBe` "_I_am_your_f4THER"
          line `shouldBe` 3
        _ -> error "Expected an identifier token"
    it "Cannot scan invalid characters in identifiers" $ do
      evaluate (Scanner.scanTokens "hi d@d") `shouldThrow` anyException
    it "Empty input should throw exception" $ do
      evaluate (Scanner.scanTokens "") `shouldThrow` anyException
    it "Input with only whitespace should throw error" $ do
      evaluate (Scanner.scanTokens " \n\r   ") `shouldThrow` anyException

  describe "Scans Correct Lines" $ do
    it "Gets some basic lines correct" $ do
      let result = map tokenToLineNumber $ Scanner.scanTokens "(){},.-+;*\n(){},.-+;*\n\n(){},.-+;*\n"
      result `shouldBe` [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5]
    it "Gets longer token line numbers correctly" $ do
      let result = map tokenToLineNumber $ Scanner.scanTokens "==\n!=\n<=\ni_am_a_line\nclass\n2398.1324\n0923."
      result `shouldBe` [1, 2, 3, 4, 5, 6, 7, 7, 7]
    it "Gets comment line numbers correctly" $ do
      let result = map tokenToLineNumber $ Scanner.scanTokens "Hi // Comment\n//\n//MORE COMMENT\nWorld"
      result `shouldBe` [1, 4, 4]
    it "Gets string line numbers correctly" $ do
      let result = map tokenToLineNumber $ Scanner.scanTokens "\"string\"\n\"multi\nline\nstring\"\"more string\""
      result `shouldBe` [1, 2, 4, 4]

  describe "Parses expressions into Statements" $ do
    it "Throws an error if empty input" $ do
      evaluate (Parser.parse []) `shouldThrow` anyException
    it "Throws an error if input list does not end with EOF" $ do
      let invalidInput = init $ Scanner.scanTokens "1 + 2;" -- Remove EOF with init
      evaluate (Parser.parse invalidInput) `shouldThrow` anyException

    it "Parses simple unary !" $ do
      "!false;" `shouldParseAs` "(!FALSE_LIT);"
      "!!true;" `shouldParseAs` "(!(!TRUE_LIT));"
    it "Parses simple unary -" $ do
      "-1;" `shouldParseAs` "(-1.0);"
      "--2;" `shouldParseAs` "(-(-2.0));"

    it "Parses basic additative" $ do
      "2 + 3 + 5;" `shouldParseAs` "((2.0 + 3.0) + 5.0);"
    it "Parses longer additative" $ do
      "1 + 2 + 3 + 4 + 5 + 6;" `shouldParseAs` "(((((1.0 + 2.0) + 3.0) + 4.0) + 5.0) + 6.0);"
    it "Parses basic multiplicative" $ do
      "2 * 3 * 5;" `shouldParseAs` "((2.0 * 3.0) * 5.0);"
    it "Parses longer multiplicative" $ do
      "1 * 2 * 3 * 4 * 5 * 6;" `shouldParseAs` "(((((1.0 * 2.0) * 3.0) * 4.0) * 5.0) * 6.0);"
    it "Parses mixed addition and multiplication basic 1" $ do
      "1 + 2 * 3;" `shouldParseAs` "(1.0 + (2.0 * 3.0));"
    it "Parses mixed addition and multiplication basic 2" $ do
      "1 * 2 + 3;" `shouldParseAs` "((1.0 * 2.0) + 3.0);"
    it "Parses mixed addition and multiplication advanced 1" $ do
      "1 + 2 * 3 + 4;" `shouldParseAs` "((1.0 + (2.0 * 3.0)) + 4.0);"
    it "Parses mixed addition and multiplication advanced 2" $ do
      "1 + 2 * 3 + 4 * 5 * 6 + 7;" `shouldParseAs` "(((1.0 + (2.0 * 3.0)) + ((4.0 * 5.0) * 6.0)) + 7.0);"

    it "Parses mixed addition and multiplication with parenthasis 1" $ do
      "(1 + 2) * 3 + 4;" `shouldParseAs` "((((1.0 + 2.0)) * 3.0) + 4.0);"
    it "Parses mixed addition and multiplication advanced 2" $ do
      "1 + 2 * (3 + 4) * (5 * 6 + 7);" `shouldParseAs` "(1.0 + ((2.0 * ((3.0 + 4.0))) * (((5.0 * 6.0) + 7.0))));"

    it "Parses simple comparisions" $ do
      "1 < 2;" `shouldParseAs` "(1.0 < 2.0);"
      "1 <= 2;" `shouldParseAs` "(1.0 <= 2.0);"
      "1 > 2;" `shouldParseAs` "(1.0 > 2.0);"
      "1 >= 2;" `shouldParseAs` "(1.0 >= 2.0);"

    it "Parses simple print" $ do
      "print 1;" `shouldParseAs` "print 1.0;"
      "print (1);" `shouldParseAs` "print (1.0);"
      "print 1 + 2;" `shouldParseAs` "print (1.0 + 2.0);"

    it "Parses multiple simple expressions" $ do
      "1 + 2; (3 + 4); !true;" `shouldParseAs` "(1.0 + 2.0);  ((3.0 + 4.0));  (! TRUE_LIT);"

    it "Throws an error when reading a single '(' 1" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "(2;") `shouldThrow` anyException
    it "Throws an error when reading a single '(' 2" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "1 + 2 * (5 + 9;") `shouldThrow` anyException
    it "Throws an error when reading more '(' than ')'" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "1 + ((1 * 1);") `shouldThrow` anyException
    it "Throws an error when reading more ')' than '('" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "1 + (1 * 1));") `shouldThrow` anyException
    it "Throws an error on expression in the middle" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "1 + 1; (4 + 4; (5 + 9);") `shouldThrow` anyException

    it "Parses empty variable declaration" $ do
      "var x;" `shouldParseAs` "V DEC -> x;"
    it "Parses empty variable declaration in the middle of statements" $ do
      "1; var x; false;" `shouldParseAs` "1.0; V DEC -> x; FALSE_LIT;"
    it "Parses simple variable declaration" $ do
      "var x = 1;" `shouldParseAs` "V DEC -> x = 1.0;"
      "var x = false;" `shouldParseAs` "V DEC -> x = FALSE_LIT;"
    it "Parses empty variable declaration in the middle of statements" $ do
      "1; var is_true = true; false;" `shouldParseAs` "1.0; V DEC -> is_true = TRUE_LIT; FALSE_LIT;"
    it "Parses empty variable declaration" $ do
      "var x = 1 + 2 * 3;" `shouldParseAs` "V DEC -> x = (1.0 + (2.0 * 3.0));"

    it "Parses simple assignment" $ do
      "gimme = x;" `shouldParseAs` "gimme = x;"
    it "Parses slighly more advanced assignment" $ do
      "gimme = (1 + 2 * 3);" `shouldParseAs` "gimme = ((1.0 + (2.0 * 3.0)));"
    it "Parses assignments between statements" $ do
      "a = b; var x; x = 0; var gimme = x; gimme = gimme; 1 + 1;" `shouldParseAs` "a = b; V DEC -> x; x = 0.0; V DEC -> gimme = x; gimme = gimme; (1.0 + 1.0);"
    it "Throws when trying to assign to rvalue" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "1 = true;") `shouldThrow` anyException
      evaluate (Parser.parse $ Scanner.scanTokens "true = false;") `shouldThrow` anyException

    it "Parses simple and" $ do
      "true and false;" `shouldParseAs` "(TRUE_LIT && FALSE_LIT);"
    it "Parses simple and" $ do
      "1 or 2;" `shouldParseAs` "(1.0 || 2.0);"
    it "Parses complex and + or" $ do
      "a or b and c or d and e;" `shouldParseAs` "((a || (b && c)) || (d && e));"

    it "Parses simple block" $ do
      "{ 1; 2; }" `shouldParseAs` "{ 1.0; 2.0; }"
      "{ a = b; }" `shouldParseAs` "{ a = b; }"
    it "Parses simple nested blocks" $ do
      "{{ 1; 2; }}" `shouldParseAs` "{{ 1.0; 2.0; }}"
      "{{{ a = b; }}}" `shouldParseAs` "{{{ a = b; }}}"
    it "Parses blocks between statements" $ do
      "1; { 1; 2; } 2+2;" `shouldParseAs` "1.0; {1.0;2.0;} (2.0 + 2.0);"
    it "Parses slightly advanced nesting" $ do
      "0;{1;{2;{3;}4;}5;}6;" `shouldParseAs` "0.0; {1.0;{2.0;{3.0;} 4.0;} 5.0;} 6.0;"
    it "Parses quite the advanced nesting" $ do
      "{ q = 0; {q = q;} 1;{x = 5;} 2; {2;} }" `shouldParseAs` "{q = 0.0;{q = q;} 1.0;{x = 5.0;} 2.0;{2.0;} }"
