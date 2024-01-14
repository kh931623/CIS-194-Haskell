module LogAnalysis
  ( parseMessage,
    parse,
    insert,
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

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ timestamp _) (Node leftTree nodeLog@(LogMessage _ nodeTimeStamp _) rightTree)
  | timestamp < nodeTimeStamp = Node (insert logMsg leftTree) nodeLog rightTree
  | otherwise = Node leftTree nodeLog (insert logMsg rightTree)
