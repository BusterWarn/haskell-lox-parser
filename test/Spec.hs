import qualified Scanner
import qualified Tokens

import Control.Exception (evaluate)
import Test.Hspec

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
    it "Throws an error for invalid characters" $ do
      evaluate (Scanner.scanTokens "{@}") `shouldThrow` anyException
