module System.Logging.LogSink.CoreSpec (main, spec) where

import           Test.Hspec

import           System.Logging.Facade.Types
import           System.Logging.LogSink.Core

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "defaultFormat" $ do
    it "converts a log record to a string" $ do
      let record = LogRecord ERROR Nothing "some message"
      defaultFormat record `shouldReturn` "ERROR: some message"
