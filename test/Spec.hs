import qualified Scanner
import qualified Tokens

import Control.Exception (evaluate)
import Test.Hspec
import Tokens (TokenType (AND, BANG, BANG_EQUAL, CLASS, COMMA, DOT, ELSE, EOF, EQUAL, EQUAL_EQUAL, FALSE, FOR, FUN, GREATER, GREATER_EQUAL, IDENTIFIER, IF, LEFT_BRACE, LEFT_PAREN, LESS, LESS_EQUAL, MINUS, NIL, NUMBER, OR, PLUS, PRINT, RETURN, RIGHT_BRACE, RIGHT_PAREN, SEMICOLON, SLASH, STAR, STRING, SUPER, THIS, TRUE, VAR, WHILE))

main :: IO ()
main = hspec tests

tokenToTokenType :: Tokens.Token -> Tokens.TokenType
tokenToTokenType (Tokens.TOKEN token _ _ _) = token

tests :: Spec
tests = do
  describe "Simple Characters Parsing" $ do
    it "Scans all simple characters" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "(){},.-+;*"
      result `shouldBe` [Tokens.LEFT_PAREN, Tokens.RIGHT_PAREN, Tokens.LEFT_BRACE, Tokens.RIGHT_BRACE, Tokens.COMMA, Tokens.DOT, Tokens.MINUS, Tokens.PLUS, Tokens.SEMICOLON, Tokens.STAR]
    it "Scans all simple characters and ignores whitespace" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "(){}\n,. -+ ;   *\r"
      result `shouldBe` [Tokens.LEFT_PAREN, Tokens.RIGHT_PAREN, Tokens.LEFT_BRACE, Tokens.RIGHT_BRACE, Tokens.COMMA, Tokens.DOT, Tokens.MINUS, Tokens.PLUS, Tokens.SEMICOLON, Tokens.STAR]
    it "Throws an error for invalid characters" $ do
      evaluate (Scanner.scanTokens "{@}") `shouldThrow` anyException
    it "Scans operator tokens" $ do
      let result = map tokenToTokenType $ Scanner.scanTokens "= ! < > == != <= >="
      result `shouldBe` [EQUAL, BANG, LESS, GREATER, EQUAL_EQUAL, BANG_EQUAL, LESS_EQUAL, GREATER_EQUAL]
