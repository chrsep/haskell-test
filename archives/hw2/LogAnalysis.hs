{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import           Log

parseMessage :: String -> LogMessage
parseMessage string = case words string of
  "I":t:m       -> LogMessage Info (read t) (unwords m)
  "W":t:m       -> LogMessage Warning (read t) (unwords m)
  "E":ecode:t:m -> LogMessage (Error $ read ecode) (read t) (unwords m)
  m             ->  Unknown (unwords m)

parse :: String -> [LogMessage]
parse logs = map parseMessage $ lines logs

--  :baby_bottle: :wink: :cinema:
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ t1 _ ) (Node l mo@(LogMessage _ t2 _ ) r)
  | t1 < t2 = Node ( insert m l ) mo r
  | t1 > t2 = Node l mo ( insert m r )
insert m Leaf = Node Leaf m Leaf
insert m _ = Node Leaf m Leaf

build :: [LogMessage] -> MessageTree
build = foldl ( flip insert ) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder ( Node l m r) = inOrder l ++ m:[] ++ inOrder r
inOrder Leaf          = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ s) -> s) . filter severe . inOrder . build
  where
    severe (LogMessage (Error n)  _ _) = n > 50
    severe _                           = False

whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' = map (\(LogMessage _ _ s) -> s) . filter severe . inOrder . build
  where
    severe (LogMessage (Error n)  _ _) = n > 50
    severe (LogMessage Info t _)       = t `elem` [120..131]
    severe _                           = False
