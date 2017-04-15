{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage string = case words string of
  "I":t:m -> LogMessage Info (read t) (unwords m)
  "W":t:m -> LogMessage Warning (read t) (unwords m)
  "E":ecode:t:m -> LogMessage (Error $ read ecode) (read t) (unwords m)
  m ->  Unknown (unwords m)

parse :: String -> [LogMessage]
parse logs = map parseMessage $ lines logs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ t1 _ ) (Node l (LogMessage _ t2 _ ) r)
  | t1 < t2 = insert m l
  | t1 > t2 = insert m r
insert m Leaf = Node Leaf m Leaf
insert m _ = Node Leaf m Leaf

{-

build :: [LogMessage] -> MessageTree

inOrder :: MessageTree -> [LogMessage]

-}
