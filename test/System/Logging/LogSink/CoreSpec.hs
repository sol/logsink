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
      defaultFormat record `shouldBe` "ERROR: some message"

    context "when log record has location information" $ do
      it "includes location information" $ do
        let record = LogRecord ERROR (Just location) "some message"
            location = Location "main" "Main" "Main.hs" 23 42
        defaultFormat record `shouldBe` "ERROR Main.hs:23:42: some message"
