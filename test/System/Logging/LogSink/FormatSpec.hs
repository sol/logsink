module System.Logging.LogSink.FormatSpec (main, spec) where

import           Test.Hspec

import           Control.Concurrent
import           System.Logging.Facade.Types
import           System.Logging.LogSink.Format

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formatLogRecord" $ do
    let record = LogRecord ERROR Nothing "some message"
    it "formats a log record" $ do
      formatLogRecord "{level}: {message}" record `shouldReturn` "ERROR: some message"

    it "interpolates thread-id" $ do
      threadId <- myThreadId
      formatLogRecord "foo {thread-id} bar" record `shouldReturn` "foo " ++ show threadId ++ " bar"

  describe "parseFormat" $ do
    it "parses format string" $ do
      parseFormat "{level}: {message}" `shouldBe` [Level, Literal ": ", Message]

    context "when given an unterminated format directive" $ do
      it "interprets it literal" $ do
        parseFormat "{level}: {.. {message}" `shouldBe` [Level, Literal ": {.. ", Message]
