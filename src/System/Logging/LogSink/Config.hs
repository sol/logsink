module System.Logging.LogSink.Config (
  SinkConfig(..)
, defaultSinkConfig
, LogLevel(..)
, LogTarget(..)
, toLogSink
, setupLogging
) where

import           Control.Applicative
import           System.Exit (exitFailure)
import           System.IO
import           System.Logging.Facade.Sink
import           System.Logging.Facade.Types

import           System.Logging.LogSink.Core
import           System.Logging.LogSink.Format
import           System.Logging.LogSink.Internal

data LogTarget = StdErr | SysLog
  deriving (Eq, Show)

data SinkConfig = SinkConfig {
  -- | Ignore all log records with a smaller log level than specified.
  sinkConfigLevel :: LogLevel
  -- | Format string for formatting log records
, sinkConfigFormat :: String
  -- | Target for log messages.
, sinkConfigTarget :: LogTarget
} deriving (Eq, Show)

-- | A sink configuration that logs all messages to `StdErr`. The used
-- format is the same as the one used by `defaultFormat`.
defaultSinkConfig :: SinkConfig
defaultSinkConfig = SinkConfig {
  sinkConfigLevel = minBound
, sinkConfigFormat = defaultFormatString
, sinkConfigTarget = StdErr
}

-- | Set up the global `LogSink` according to the given sink configurations.
setupLogging :: [SinkConfig] -> IO ()
setupLogging sinks = either die (setLogSink . combine) (mapM toLogSink sinks)

die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

-- | Convert the given sink configuration to a `LogSink`. Return @Left@ if
-- the format string of the configuration is invalid.
toLogSink :: SinkConfig -> Either String LogSink
toLogSink sink = filterByLogLevel (sinkConfigLevel sink) . targetToSink sink <$> parseFormat_ (sinkConfigFormat sink)
  where
    parseFormat_ :: String -> Either String Format
    parseFormat_ fmt = case parseFormat fmt of
      Left err -> Left ("Invalid format " ++ show fmt ++ " (" ++ err ++ ")")
      Right f -> return f

targetToSink :: SinkConfig -> Format -> LogSink
targetToSink sink = case sinkConfigTarget sink of
  StdErr -> stdErrSink
  SysLog -> sysLogSink
