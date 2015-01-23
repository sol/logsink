{-# LANGUAGE RecordWildCards #-}
module System.Logging.LogSink.Internal where

import           Control.Applicative
import           Control.Concurrent
import           Data.Char
import           Data.Time.Clock
import           Data.Time.LocalTime ()
import           System.Logging.Facade.Types

type Format = LogRecord -> IO String

defaultFormatString :: String
defaultFormatString = "{level}: {message}"

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

parseNodes :: String -> Either String [Node]
parseNodes = fmap (filter $ not . isEmpty) . go ""
  where
    isIdChar :: Char -> Bool
    isIdChar c = isAlphaNum c || (c `elem` "-_")

    lookupNode :: String -> Maybe Node
    lookupNode key = lookup key [("level", Level), ("message", Message), ("timestamp", Timestamp), ("thread-id", ThreadId)]

    go :: String -> String -> Either String [Node]
    go acc input = case input of
      ""  -> return [lit acc]
      '{':xs | (key,'}':ys) <- span isIdChar xs -> case lookupNode key of
        Nothing -> Left ("invalid format directive " ++ show key)
        Just node -> do
          nodes <- go "" ys
          return (lit acc : node : nodes)
      x:xs -> go (x:acc) xs

    lit :: String -> Node
    lit acc = (Literal . reverse) acc

    isEmpty :: Node -> Bool
    isEmpty (Literal "") = True
    isEmpty _ = False
