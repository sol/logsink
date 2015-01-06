{-# LANGUAGE RecordWildCards #-}
module System.Logging.LogSink.Format where

import           Control.Applicative
import           Control.Concurrent
import           Data.Time.Clock
import           Data.Time.LocalTime ()
import           System.Logging.Facade.Types
import           System.Logging.LogSink.Core

formatLogRecord :: String -> Format
formatLogRecord format = formatNodes (parseFormat format)

data Node = Level | Message | Timestamp | ThreadId | Literal String
  deriving (Eq, Show)

formatNodes :: [Node] -> LogRecord -> IO String
formatNodes nodes LogRecord{..} = concat <$> mapM evalNode nodes
  where
    evalNode :: Node -> IO String
    evalNode node = case node of
      Level -> return (show logRecordLevel)
      Message -> return logRecordMessage
      Timestamp -> show <$> getCurrentTime
      ThreadId -> show <$> myThreadId
      Literal s -> return s

parseFormat :: String -> [Node]
parseFormat = filter (not . isEmpty) . go ""
  where
    isIdChar :: Char -> Bool
    isIdChar = (`elem` "abcdefghijklmnopqrstuvwxyz-")

    lookupNode :: String -> Maybe Node
    lookupNode key = lookup key [("level", Level), ("message", Message), ("timestamp", Timestamp), ("thread-id", ThreadId)]

    go :: String -> String -> [Node]
    go acc input = case input of
      ""  -> [lit acc]
      '#':'{':xs | (key,'}':ys) <- span isIdChar xs, Just node <- lookupNode key -> lit acc : node : go "" ys
      x:xs -> go (x:acc) xs

    lit :: String -> Node
    lit acc = (Literal . reverse) acc

    isEmpty :: Node -> Bool
    isEmpty (Literal "") = True
    isEmpty _ = False
