module System.Logging.LogSink.Core (
  Format
, defaultFormat
, stdErrSink
, sysLogSink
, combine
, filterByLogLevel
) where

import           Control.Concurrent.MVar
import           Control.Monad
import           System.IO
import           System.IO.Unsafe
import           System.Logging.Facade.Sink
import           System.Logging.Facade.Types
import           System.Posix.Syslog

import           System.Logging.LogSink.Format
import           System.Logging.LogSink.Internal
import           Foreign.C.String

-- | Default format function that formats log records like so:
-- > {level}: {message}
defaultFormat :: Format
defaultFormat =
  let Right format = parseFormat defaultFormatString
  in format

{-# NOINLINE stderrLock #-}
stderrLock :: MVar ()
stderrLock = unsafePerformIO $ newMVar ()

stdErrSink :: Format -> LogSink
stdErrSink format record = do
  s <- format record
  modifyMVar_ stderrLock $ \ () -> hPutStrLn stderr s

sysLogSink :: Format -> LogSink
sysLogSink format record = do
  str <- format record
  withCStringLen str (syslog Nothing (toPriority $ logRecordLevel record))
    where
      toPriority :: LogLevel -> Priority
      toPriority l = case l of
        TRACE -> Debug
        DEBUG -> Debug
        INFO -> Info
        WARN -> Warning
        ERROR -> Error

combine :: [LogSink] -> LogSink
combine sinks record = do
  forM_ sinks $ \sink -> sink record

filterByLogLevel :: LogLevel -> LogSink -> LogSink
filterByLogLevel level sink
  | level == minBound = sink
  | otherwise = filteringSink
  where
    filteringSink record
      | logRecordLevel record < level = return ()
      | otherwise = sink record
