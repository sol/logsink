module System.Logging.LogSink.FormatSpec (main, spec) where

import           Test.Hspec

import           Control.Concurrent
import           System.Logging.Facade.Types
import           System.Logging.LogSink.Format

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseFormat" $ do
    let record = LogRecord ERROR Nothing "some message"
    it "formats a log record" $ do
      let Right format = parseFormat "{level}: {message}"
      format record `shouldReturn` "ERROR: some message"

    it "interpolates thread-id" $ do
      threadId <- myThreadId
      let Right format = parseFormat "foo {thread-id} bar"
      format record `shouldReturn` "foo " ++ show threadId ++ " bar"

  describe "parseNodes" $ do
    it "parses format string" $ do
      parseNodes "{level}: {message}" `shouldBe` Right [Level, Literal ": ", Message]

    context "when given an unterminated format directive" $ do
      it "interprets it literal" $ do
        parseNodes "{level}: {.. {message}" `shouldBe` Right [Level, Literal ": {.. ", Message]

    context "when given an unknown format directive" $ do
      it "returns Left" $ do
        parseNodes "foo {bar} baz" `shouldBe` Left "invalid format directive \"bar\""
