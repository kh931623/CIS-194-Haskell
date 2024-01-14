module LogAnalysis
  ( parseMessage,
    parse,
    insert,
    build,
    inOrder,
    whatWentWrong,
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

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree logMsg rightTree) = inOrder leftTree ++ logMsg : inOrder rightTree

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error level) _ _) = level >= 50
isSevereError _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ str) = str
getMessage (Unknown _) = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . filter isSevereError . inOrder . build
