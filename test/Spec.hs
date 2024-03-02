import qualified Parser
import qualified Scanner
import qualified Tokens

import Control.Exception (evaluate)
import Test.Hspec
import Tokens (TokenType (AND, BANG, BANG_EQUAL, CLASS, COMMA, DOT, ELSE, EOF, EQUAL, EQUAL_EQUAL, FALSE, FOR, FUN, GREATER, GREATER_EQUAL, IDENTIFIER, IF, LEFT_BRACE, LEFT_PAREN, LESS, LESS_EQUAL, MINUS, NIL, NUMBER, OR, PLUS, PRINT, RETURN, RIGHT_BRACE, RIGHT_PAREN, SEMICOLON, SLASH, STAR, STRING, SUPER, THIS, TRUE, VAR, WHILE))

main :: IO ()
main = hspec tests

tokenToTokenType :: Tokens.Token -> Tokens.TokenType
tokenToTokenType (Tokens.TOKEN token _ _ _) = token

tokenToLineNumber :: Tokens.Token -> Int
tokenToLineNumber (Tokens.TOKEN _ _ _ line) = line

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

  describe "Parses expressions into AST" $ do
    it "Parses simple unary !" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "!false"
      result `shouldBe` "(!FALSE_LIT)"
      let result = show . Parser.parse $ Scanner.scanTokens "!!true"
      result `shouldBe` "(!(!TRUE_LIT))"
    it "Parses simple unary -" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "-1"
      result `shouldBe` "(-1.0)"
      let result = show . Parser.parse $ Scanner.scanTokens "--2"
      result `shouldBe` "(-(-2.0))"

    it "Parses basic additative" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "2 + 3 + 5"
      result `shouldBe` "((2.0 + 3.0) + 5.0)"
    it "Parses longer additative" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "1 + 2 + 3 + 4 + 5 + 6"
      result `shouldBe` "(((((1.0 + 2.0) + 3.0) + 4.0) + 5.0) + 6.0)"
    it "Parses basic multiplicative" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "2 * 3 * 5"
      result `shouldBe` "((2.0 * 3.0) * 5.0)"
    it "Parses longer multiplicative" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "1 * 2 * 3 * 4 * 5 * 6"
      result `shouldBe` "(((((1.0 * 2.0) * 3.0) * 4.0) * 5.0) * 6.0)"
    it "Parses mixed addition and multiplication basic 1" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "1 + 2 * 3"
      result `shouldBe` "(1.0 + (2.0 * 3.0))"
    it "Parses mixed addition and multiplication basic 2" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "1 * 2 + 3"
      result `shouldBe` "((1.0 * 2.0) + 3.0)"
    it "Parses mixed addition and multiplication advanced 1" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "1 + 2 * 3 + 4"
      result `shouldBe` "((1.0 + (2.0 * 3.0)) + 4.0)"
    it "Parses mixed addition and multiplication advanced 2" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "1 + 2 * 3 + 4 * 5 * 6 + 7"
      result `shouldBe` "(((1.0 + (2.0 * 3.0)) + ((4.0 * 5.0) * 6.0)) + 7.0)"
    it "Parses mixed addition and multiplication with parenthasis 1" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "(1 + 2) * 3 + 4"
      result `shouldBe` "(((1.0 + 2.0) * 3.0) + 4.0)"
    it "Parses mixed addition and multiplication advanced 2" $ do
      let result = show . Parser.parse $ Scanner.scanTokens "1 + 2 * (3 + 4) * (5 * 6 + 7)"
      result `shouldBe` "(1.0 + ((2.0 * (3.0 + 4.0)) * ((5.0 * 6.0) + 7.0)))"
