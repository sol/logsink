module System.Logging.LogSink.Core (
  Config(..)
, Format
, defaultFormat
, stdErrSink
, sysLogSink
, filterByLogLevel
, mkLogSink
) where

import           Control.Monad
import           System.IO
import           System.Posix.Syslog
import           System.Logging.Facade.Types
import           System.Logging.Facade.Sink

data Config = Config {
  configSinks :: [LogSink]
}

type Format = LogRecord -> IO String

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

sysLogSink :: Format -> LogSink
sysLogSink format record = format record >>= syslog (toPriority $ logRecordLevel record)
  where
    toPriority :: LogLevel -> Priority
    toPriority l = case l of
      TRACE -> Debug
      DEBUG -> Debug
      INFO -> Info
      WARN -> Warning
      ERROR -> Error

stdErrSink :: Format -> LogSink
stdErrSink format record = format record >>= hPutStrLn stderr

filterByLogLevel :: LogLevel -> LogSink -> LogSink
filterByLogLevel level sink record
  | logRecordLevel record < level = return ()
  | otherwise = sink record

mkLogSink :: Config -> LogSink
mkLogSink (Config sinks) record = do
  forM_ sinks $ \sink -> sink record
