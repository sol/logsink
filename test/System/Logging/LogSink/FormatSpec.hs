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
