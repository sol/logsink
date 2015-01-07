module System.Logging.LogSink (
  Sink(..)
, LogLevel(..)
, LogTarget(..)
, setupLogging
) where

import           Control.Applicative
import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

import qualified System.Logging.Facade as Log

import           System.Logging.LogSink.Core
import           System.Logging.LogSink.Format

data LogTarget = StdErr | SysLog
  deriving (Eq, Show)

data Sink = Sink {
  sinkLevel :: Maybe LogLevel
, sinkFormat :: Maybe String
, sinkTarget :: LogTarget
} deriving (Eq, Show)

setupLogging :: [Sink] -> IO ()
setupLogging sinks = mapM toLogSink sinks >>= setLogSink . combine

toLogSink :: Sink -> IO LogSink
toLogSink sink = maybe id filterByLogLevel (sinkLevel sink) . targetToSink sink <$> format
  where
    format :: IO Format
    format = maybe (return defaultFormat) parseFormat_ (sinkFormat sink)

    parseFormat_ :: String -> IO Format
    parseFormat_ fmt = case parseFormat fmt of
      Left err -> do
        Log.warn ("Invalid format " ++ show fmt ++ " (" ++ err ++ ")")
        return defaultFormat
      Right f -> return f

targetToSink :: Sink -> Format -> LogSink
targetToSink sink = case sinkTarget sink of
  StdErr -> stdErrSink
  SysLog -> sysLogSink
