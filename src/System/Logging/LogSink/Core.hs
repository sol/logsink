module System.Logging.LogSink.Core (
  Config(..)
, Format
, Sink
, defaultFormat
, stdErrSink
, sysLogSink
, mkLogSink
) where

import           Control.Monad
import           System.IO
import           System.Posix.Syslog
import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

data Config = Config {
  configFormat :: Format
, configSinks :: [Sink]
}

type Format = LogRecord -> IO String

type Sink = LogLevel -> String -> IO ()

defaultFormat :: Format
defaultFormat record = return $ (shows level . location . showString ": " . showString message) ""
  where
    level = logRecordLevel record
    mLocation = logRecordLocation record
    message = logRecordMessage record
    location = maybe (showString "") ((showString " " .) . formatLocation) mLocation

    formatLocation :: Location -> ShowS
    formatLocation loc = showString (locationFile loc) . colon . shows (locationLine loc) . colon . shows (locationColumn loc)
      where colon = showString ":"

sysLogSink :: Sink
sysLogSink level = syslog (toPriority level)
  where
    toPriority :: LogLevel -> Priority
    toPriority l = case l of
      TRACE -> Debug
      DEBUG -> Debug
      INFO -> Info
      WARN -> Warning
      ERROR -> Error

stdErrSink :: Sink
stdErrSink _level = hPutStrLn stderr

mkLogSink :: Config -> LogSink
mkLogSink (Config format sinks) logRecord = do
  output <- format logRecord
  forM_ sinks $ \sink -> sink level output
  where
    level = logRecordLevel logRecord
