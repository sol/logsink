{-# LANGUAGE RecordWildCards #-}
module System.Logging.LogSink.Format where

import           System.Logging.Facade.Types
import           System.Logging.LogSink.Core

formatLogRecord :: String -> Format
formatLogRecord format = formatNodes (parseFormat format)

data Node = Level | Message | Literal String
  deriving (Eq, Show)

formatNodes :: [Node] -> LogRecord -> String
formatNodes nodes LogRecord{..} = concat (map evalNode nodes)
  where
    evalNode :: Node -> String
    evalNode node = case node of
      Level -> show logRecordLevel
      Message -> logRecordMessage
      Literal s -> s

parseFormat :: String -> [Node]
parseFormat = filter (not . isEmpty) . go ""
  where
    isIdChar :: Char -> Bool
    isIdChar = (`elem` "abcdefghijklmnopqrstuvwxyz")

    lookupNode :: String -> Maybe Node
    lookupNode key = lookup key [("level", Level), ("message", Message)]
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
