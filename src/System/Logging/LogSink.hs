module System.Logging.LogSink (
  Sink(..)
, LogTarget(..)
, LogLevel(..)
, setupLogging
) where

import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

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
setupLogging = setLogSink . combine . map toLogSink

toLogSink :: Sink -> LogSink
toLogSink sink = maybe id filterByLogLevel (sinkLevel sink) $ case sinkTarget sink of
  StdErr -> stdErrSink format
  SysLog -> sysLogSink format
  where
    format = maybe defaultFormat formatLogRecord (sinkFormat sink)
