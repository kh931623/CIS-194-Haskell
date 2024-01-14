module LogAnalysis
  ( parseMessage,
    parse,
  )
where

import Log

parseMessage :: String -> LogMessage
parseMessage str = case splitedWords of
  ("E" : level : timestamp : xs) -> LogMessage (Error $ read level) (read timestamp) (unwords xs)
  ("I" : timestamp : xs) -> LogMessage Info (read timestamp) (unwords xs)
  ("W" : timestamp : xs) -> LogMessage Warning (read timestamp) (unwords xs)
  _ -> Unknown str
  where
    splitedWords = words str

parse :: String -> [LogMessage]
parse = map parseMessage . lines
