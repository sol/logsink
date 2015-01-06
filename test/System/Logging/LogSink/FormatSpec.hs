module System.Logging.LogSink.FormatSpec (main, spec) where

import           Test.Hspec

import           System.Logging.Facade.Types
import           System.Logging.LogSink.Format

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "formatLogRecord" $ do
    it "formats a log record" $ do
      let record = LogRecord ERROR Nothing "some message"
      formatLogRecord "#{level}: #{message}" record `shouldBe` "ERROR: some message"

  describe "parseFormat" $ do
    it "parses format string" $ do
      parseFormat "#{level}: #{message}" `shouldBe` [Level, Literal ": ", Message]

    context "when given an unterminated format directive" $ do
      it "interprets it literal" $ do
        parseFormat "#{level}: #{.. #{message}" `shouldBe` [Level, Literal ": #{.. ", Message]
