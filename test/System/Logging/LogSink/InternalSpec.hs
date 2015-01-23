module System.Logging.LogSink.InternalSpec (main, spec) where

import           Test.Hspec

import           System.Logging.LogSink.Internal

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseNodes" $ do
    it "parses format string" $ do
      parseNodes "{level}: {message}" `shouldBe` Right [Level, Literal ": ", Message]

    context "when given an unterminated format directive" $ do
      it "interprets it literal" $ do
        parseNodes "{level}: {.. {message}" `shouldBe` Right [Level, Literal ": {.. ", Message]

    context "when given an unknown format directive" $ do
      it "returns Left" $ do
        parseNodes "foo {bar} baz" `shouldBe` Left "invalid format directive \"bar\""
