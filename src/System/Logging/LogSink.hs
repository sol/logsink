module System.Logging.LogSink (
  Sink(..)
, LogLevel(..)
, LogTarget(..)
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
      Left err -> die ("Invalid format " ++ show fmt ++ " (" ++ err ++ ")")
      Right f -> return f

die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure

targetToSink :: Sink -> Format -> LogSink
targetToSink sink = case sinkTarget sink of
  StdErr -> stdErrSink
  SysLog -> sysLogSink
