import qualified Interpreter
import qualified Parser
import qualified Scanner
import qualified Tokens

import Control.Exception (evaluate)
import Data.Char (isSpace)
import Data.Either (fromRight, isLeft)
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

-- Custom infix function for testing parser
shouldParseAs :: String -> String -> Expectation
input `shouldParseAs` expected =
  let parsedStatements = Parser.parse $ Scanner.scanTokens input
      actualShowString = show parsedStatements
      actualNoWhitespace = removeWhitespace actualShowString
      expectedNoWhitespace = removeWhitespace expected
   in actualNoWhitespace `shouldBe` expectedNoWhitespace

-- Custom infix function for testing interpreter
shouldInterpretAs :: String -> Either String [String] -> Expectation
shouldInterpretAs code expectedResult =
  let result = Interpreter.interpret code
   in case expectedResult of
        Left expectedError -> do
          result `shouldSatisfy` isLeft
        Right expectedOutput -> do
          let actualOutput = snd $ fromRight (error "Expected Right, found Left") result
          actualOutput `shouldBe` expectedOutput
 where
  fromLeft :: a -> Either a b -> a
  fromLeft _ (Left a) = a
  fromLeft defaultValue _ = defaultValue

  fromRight :: b -> Either a b -> b
  fromRight _ (Right b) = b
  fromRight defaultValue _ = defaultValue

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
      "!false;" `shouldParseAs` "1 (!FALSE_LIT);"
      "!!true;" `shouldParseAs` "1 (!(!TRUE_LIT));"
    it "Parses simple unary -" $ do
      "-1;" `shouldParseAs` "1 (-1.0);"
      "--2;" `shouldParseAs` "1 (-(-2.0));"

    it "Parses basic additative" $ do
      "2 + 3 + 5;" `shouldParseAs` "1 ((2.0 + 3.0) + 5.0);"
    it "Parses longer additative" $ do
      "1 + 2 + 3 + 4 + 5 + 6;" `shouldParseAs` "1 (((((1.0 + 2.0) + 3.0) + 4.0) + 5.0) + 6.0);"
    it "Parses basic multiplicative" $ do
      "2 * 3 * 5;" `shouldParseAs` "1 ((2.0 * 3.0) * 5.0);"
    it "Parses longer multiplicative" $ do
      "1 * 2 * 3 * 4 * 5 * 6;" `shouldParseAs` "1 (((((1.0 * 2.0) * 3.0) * 4.0) * 5.0) * 6.0);"
    it "Parses mixed addition and multiplication basic 1" $ do
      "1 + 2 * 3;" `shouldParseAs` "1 (1.0 + (2.0 * 3.0));"
    it "Parses mixed addition and multiplication basic 2" $ do
      "1 * 2 + 3;" `shouldParseAs` "1 ((1.0 * 2.0) + 3.0);"
    it "Parses mixed addition and multiplication advanced 1" $ do
      "1 + 2 * 3 + 4;" `shouldParseAs` "1 ((1.0 + (2.0 * 3.0)) + 4.0);"
    it "Parses mixed addition and multiplication advanced 2" $ do
      "1 + 2 * 3 + 4 * 5 * 6 + 7;" `shouldParseAs` "1 (((1.0 + (2.0 * 3.0)) + ((4.0 * 5.0) * 6.0)) + 7.0);"

    it "Parses mixed addition and multiplication with parenthasis 1" $ do
      "(1 + 2) * 3 + 4;" `shouldParseAs` "1 ((((1.0 + 2.0)) * 3.0) + 4.0);"
    it "Parses mixed addition and multiplication advanced 2" $ do
      "1 + 2 * (3 + 4) * (5 * 6 + 7);" `shouldParseAs` "1 (1.0 + ((2.0 * ((3.0 + 4.0))) * (((5.0 * 6.0) + 7.0))));"

    it "Parses simple comparisions" $ do
      "1 < 2;" `shouldParseAs` "1 (1.0 < 2.0);"
      "1 <= 2;" `shouldParseAs` "1 (1.0 <= 2.0);"
      "1 > 2;" `shouldParseAs` "1 (1.0 > 2.0);"
      "1 >= 2;" `shouldParseAs` "1 (1.0 >= 2.0);"

    it "Parses simple print" $ do
      "print 1;" `shouldParseAs` "1 print 1.0;"
      "print (1);" `shouldParseAs` "1 print (1.0);"
      "print 1 + 2;" `shouldParseAs` "1 print (1.0 + 2.0);"

    it "Parses multiple simple expressions" $ do
      "1 + 2; (3 + 4); !true;" `shouldParseAs` "3 (1.0 + 2.0);  ((3.0 + 4.0));  (! TRUE_LIT);"

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
      "var x;" `shouldParseAs` "1 V DEC -> x;"
    it "Parses empty variable declaration in the middle of statements" $ do
      "1; var x; false;" `shouldParseAs` "3 1.0; V DEC -> x; FALSE_LIT;"
    it "Parses simple variable declaration" $ do
      "var x = 1;" `shouldParseAs` "1 V DEC -> x = 1.0;"
      "var x = false;" `shouldParseAs` "1 V DEC -> x = FALSE_LIT;"
    it "Parses empty variable declaration in the middle of statements" $ do
      "1; var is_true = true; false;" `shouldParseAs` "3 1.0; V DEC -> is_true = TRUE_LIT; FALSE_LIT;"
    it "Parses empty variable declaration" $ do
      "var x = 1 + 2 * 3;" `shouldParseAs` "1 V DEC -> x = (1.0 + (2.0 * 3.0));"
    it "Parses variable string" $ do
      "var greet = \"Hello World!\";" `shouldParseAs` "1 V DEC -> greet = \" Hello World ! \";"
    it "Parses empty const declaration" $ do
      "const x;" `shouldParseAs` "1 C DEC -> x;"
    it "Parses simple const declaration" $ do
      "const x = 1;" `shouldParseAs` "1 C DEC -> x = 1.0;"
      "const x = false;" `shouldParseAs` "1 C DEC -> x = FALSE_LIT;"

    it "Parses simple assignment" $ do
      "gimme = x;" `shouldParseAs` "1 gimme = x;"
    it "Parses slighly more advanced assignment" $ do
      "gimme = (1 + 2 * 3);" `shouldParseAs` "1 gimme = ((1.0 + (2.0 * 3.0)));"
    it "Parses assignments between statements" $ do
      "a = b; var x; x = 0; var gimme = x; gimme = gimme; 1 + 1;" `shouldParseAs` "6 a = b; V DEC -> x; x = 0.0; V DEC -> gimme = x; gimme = gimme; (1.0 + 1.0);"
    it "Throws when trying to assign to rvalue" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "1 = true;") `shouldThrow` anyException
      evaluate (Parser.parse $ Scanner.scanTokens "true = false;") `shouldThrow` anyException

    it "Parses simple and" $ do
      "true and false;" `shouldParseAs` "1 (TRUE_LIT && FALSE_LIT);"
    it "Parses simple and" $ do
      "1 or 2;" `shouldParseAs` "1 (1.0 || 2.0);"
    it "Parses complex and + or" $ do
      "a or b and c or d and e;" `shouldParseAs` "1 ((a || (b && c)) || (d && e));"

    it "Parses simple block" $ do
      "{ 1; 2; }" `shouldParseAs` "1 { 1.0; 2.0; }"
      "{ a = b; }" `shouldParseAs` "1 { a = b; }"
    it "Parses simple nested blocks" $ do
      "{{ 1; 2; }}" `shouldParseAs` "1 {{ 1.0; 2.0; }}"
      "{{{ a = b; }}}" `shouldParseAs` "1 {{{ a = b; }}}"
    it "Parses blocks between statements" $ do
      "1; { 1; 2; } 2+2;" `shouldParseAs` "3 1.0; {1.0;2.0;} (2.0 + 2.0);"
    it "Parses slightly advanced nesting" $ do
      "0;{1;{2;{3;}4;}5;}6;" `shouldParseAs` "3 0.0; {1.0;{2.0;{3.0;} 4.0;} 5.0;} 6.0;"
    it "Parses quite the advanced nesting" $ do
      "{ q = 0; {q = q;} 1;{x = 5;} 2; {2;} }" `shouldParseAs` "1 {q = 0.0;{q = q;} 1.0;{x = 5.0;} 2.0;{2.0;} }"

    it "Parses simple if statements" $ do
      "if (true) false;" `shouldParseAs` "1 if (TRUE_LIT) FALSE_LIT;"
    it "Parses simple if else statements" $ do
      "if (false) x = 1; else x = 2;" `shouldParseAs` "1 if (FALSE_LIT) x = 1.0; else x = 2.0;"
    it "Parses simple var dec inside if block statement." $ do
      "if (summer) { var ice_cream = hot; }" `shouldParseAs` "1 if (summer) { V DEC -> ice_cream = hot; }"
    it "Parses simple if with block" $ do
      "if (1 == 2) { i = ice_cream; }" `shouldParseAs` "1 if ((1.0 == 2.0)) { i = ice_cream; }"
    it "Parses declaration inside both if and else blocks" $ do
      "if (true) { var x = y; } else { var y = x; }" `shouldParseAs` "1 if (TRUE_LIT) {V DEC -> x = y;}  else {V DEC -> y = x;}"
    it "Parses simple if else with block" $ do
      "if (summer) { i = coke; coke = i; } else { outside = coke + true and false; }" `shouldParseAs` "1 if (summer) {i = coke;coke = i;}  else {outside = ((coke + TRUE_LIT) && FALSE_LIT); }"
    it "Parses nested if else statements" $ do
      "if (hawaii) { if (sun) shirt = on; else false; } else if (greece) if (gyros) print tacos; else print \"fries\";" `shouldParseAs` "1 if (hawaii) {if (sun) shirt = on; else FALSE_LIT;}  else if (greece) if (gyros) print tacos; else print \"fries\";"

    it "Parser throws error if you forget () around if condition" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "if x x = x; ") `shouldThrow` anyException
    it "Parser throws error if you forget ; inside if block { }" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "if (x) { x = x } ") `shouldThrow` anyException
    it "Parser thows error if var decl is outside of if block" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "if (x) var y = x; ") `shouldThrow` anyException
    it "Parser thows error if var decl is outside of else block" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "if (x) y = x; else var x = y;") `shouldThrow` anyException

    it "Parses simple while statement" $ do
      "while (1) fork;" `shouldParseAs` "1 while (1.0) fork;"
    it "Parses simple while statement with block" $ do
      "while (1 != 2) { fork; }" `shouldParseAs` "1 while (( 1.0 != 2.0)) { fork; }"
    it "Parses simple while loop with if and else and assignments" $ do
      "while ( x == true ) if (false) x = false; else x = true;" `shouldParseAs` "1 while ((x == TRUE_LIT)) if (FALSE_LIT) x = FALSE_LIT; else x = TRUE_LIT;"
    it "Parses simple while loop if and else and vardecl inside" $ do
      "while ( x == true ) if (false) { var x = false; } else { var x = true; }" `shouldParseAs` "1 while ((x == TRUE_LIT))if (FALSE_LIT) { V DEC -> x = FALSE_LIT; } else { V DEC -> x = TRUE_LIT; }"

    it "Parser throws error if you forget () around while condition" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "while x x = x; ") `shouldThrow` anyException
    it "Parser throws error if you forget ; inside while block { }" $ do
      evaluate (Parser.parse $ Scanner.scanTokens "while (x) { x = x } ") `shouldThrow` anyException

    it "Parses simple return" $ do
      "return;" `shouldParseAs` "1 return;"
    it "Parses pretty advanced example 1" $ do
      "if ( a < 5 ) { print g; 88; } else { if (false) { while (a=5) return; } }" `shouldParseAs` "1 if((a<5.0)){printg;88.0;}else{if(FALSE_LIT){while(a=5.0)return;}}"
    it "Parses pretty advanced example 2" $ do
      "if (i_can_sing) { you_can_dance = 2; const x = 7; } else print hi; while (1 != 2) {return;} {{ 1; }}" `shouldParseAs` "3 if (i_can_sing) { you_can_dance = 2.0; C DEC -> x = 7.0; } else print hi; while ((1.0 != 2.0)){ return; } { { 1.0; } }"

  describe "Interpret code" $ do
    it "Can interperet single num" $ do
      "print 1;" `shouldInterpretAs` Right ["1"]
      "print 2.0;" `shouldInterpretAs` Right ["2"]
    it "can interperet num unary -" $ do
      "print -1;" `shouldInterpretAs` Right ["-1"]
      "print -2.0;" `shouldInterpretAs` Right ["-2"]
    it "can interperet binary plus expression" $ do
      "print 10 + 2;" `shouldInterpretAs` Right ["12"]
    it "can interperet binary minus expression" $ do
      "print 10 -  2;" `shouldInterpretAs` Right ["8"]
    it "can interperet binary star expression" $ do
      "print 10 * 2;" `shouldInterpretAs` Right ["20"]
    it "can interperet binary slash expression" $ do
      "print 10 / 2;" `shouldInterpretAs` Right ["5"]
    it "can interperet multiple binary number operations" $ do
      "print 10 * 2 - 5 / 5 + -1 * 10;" `shouldInterpretAs` Right ["9"]
    it "can interperet simple string binary addition" $ do
      "print \"hello \" + \"world!\";" `shouldInterpretAs` Right ["hello world!"]
    it "can interperet multple string binary addition" $ do
      "print \"1\" + \"2\"+ \"3\"+ \"4\";" `shouldInterpretAs` Right ["1234"]
    it "can interperet simple bool literal" $ do
      "print true;" `shouldInterpretAs` Right ["true"]
      "print false;" `shouldInterpretAs` Right ["false"]
    it "can interperet simple bool literal with unary bang" $ do
      "print !true;" `shouldInterpretAs` Right ["false"]
      "print !false;" `shouldInterpretAs` Right ["true"]

  describe "Interpreter will return runtime errors" $ do
    it "Returns runtime error when multiplying with muffin" $ do
      "1 * \"muffin\";" `shouldInterpretAs` Left ""
      "\"muffin\" * 1;" `shouldInterpretAs` Left ""
      "1 * tshouldInterpretAs;" `shouldInterpretAs` Left ""
      "false * 1;" `shouldInterpretAs` Left ""
    it "Returns runtime error when dividing with muffin" $ do
      "1 / \"muffin\";" `shouldInterpretAs` Left ""
      "\"muffin\" / 1;" `shouldInterpretAs` Left ""
      "1 / true;" `shouldInterpretAs` Left ""
      "false / 1;" `shouldInterpretAs` Left ""
    it "Returns runtime error when subtracting with muffin" $ do
      "1 - \"muffin\";" `shouldInterpretAs` Left ""
      "\"muffin\" - 1;" `shouldInterpretAs` Left ""
      "1 - true;" `shouldInterpretAs` Left ""
      "false - 1;" `shouldInterpretAs` Left ""
    it "Returns runtime error when adding number with muffin" $ do
      "1 + \"muffin\";" `shouldInterpretAs` Left ""
      "\"muffin\" + 1;" `shouldInterpretAs` Left ""
      "1 + true;" `shouldInterpretAs` Left ""
      "false + 1;" `shouldInterpretAs` Left ""
    it "Returns runtime error when dividing by 0" $ do
      "1 / 0;" `shouldInterpretAs` Left ""

    it "load and evaluate simple variable" $ do
      "var x = 5; print x;" `shouldInterpretAs` Right ["5"]
      "const x = 5; print x;" `shouldInterpretAs` Right ["5"]
    it "Load multiple variables" $ do
      "var ten = 10; const seven = 7; var two = 2; print ten * two - 7;" `shouldInterpretAs` Right ["13"]
    it "Load multiple variables referencing each other" $ do
      "var x = 5; var y = x; var z = y; print z;" `shouldInterpretAs` Right ["5"]
    it "Can redeclare variables" $ do
      "var x = 5; var x = x * 10; print x;" `shouldInterpretAs` Right ["50"]
    it "Can redeclare constant variables" $ do
      "const x = 5; const x = x * 10; print x;" `shouldInterpretAs` Right ["50"]

    it "Can first declare and then assign variables" $ do
      "var x; x = 42; print x;" `shouldInterpretAs` Right ["42"]
      "const x; x = 42; print x;" `shouldInterpretAs` Right ["42"]
    it "Can redaclare variable multiple times" $ do
      "var x = 1; print x; x = 2; print x; x = x + 1; print x;" `shouldInterpretAs` Right ["1", "2", "3"]
    it "Can reassign different types to a variable" $ do
      "var x = 5; print x; x = \"string\"; print x; x = true;  print x; x = nil; print x;" `shouldInterpretAs` Right ["5", "string", "true", "nil"]

    it "Cannot assign value from unassigned variable." $ do
      "var x; var y = x;" `shouldInterpretAs` Left ""
    it "Cannot assign a value to an already assigned constant varaible" $ do
      "const x = 42; x = 43;" `shouldInterpretAs` Left ""
      "const x; x = 1; x = 2;" `shouldInterpretAs` Left ""
