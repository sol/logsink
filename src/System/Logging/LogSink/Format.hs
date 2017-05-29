{-# LANGUAGE RecordWildCards #-}
module System.Logging.LogSink.Format (parseFormat) where

import           Prelude ()
import           System.Logging.LogSink.Compat

import           System.Logging.LogSink.Internal

-- | Parses a format string into a `Format` function.
--
-- It's possible to include information about the log record and the
-- context in which logging happened into the formatted message. The
-- following variables can be interpolated by enclosing them in curly braces:
--
-- [level] The `System.Logging.Facade.LogLevel` of the log record.
-- [message] The actual log message of the log record.
-- [timestamp] The time at which the log record is consumed.
-- [thread-id] The `Control.Concurrent.ThreadId` of the logging thread.
--
-- For example, @"{timestamp} {level}: {message}"@ would be a valid format
-- string.
--
-- When given an invalid format strings, @parseFormat@ returns @Left@.
parseFormat :: String -> Either String Format
parseFormat format = formatNodes <$> parseNodes format
