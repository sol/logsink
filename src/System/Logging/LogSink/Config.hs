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

data LogTarget = StdErr | SysLog
  deriving (Eq, Show)

data SinkConfig = SinkConfig {
  sinkConfigLevel :: LogLevel
, sinkConfigFormat :: String
, sinkConfigTarget :: LogTarget
} deriving (Eq, Show)

defaultSinkConfig :: SinkConfig
defaultSinkConfig = SinkConfig {
  sinkConfigLevel = minBound
, sinkConfigFormat = defaultFormatString
, sinkConfigTarget = StdErr
}

setupLogging :: [SinkConfig] -> IO ()
setupLogging sinks = either die (setLogSink . combine) (mapM toLogSink sinks)

die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

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
