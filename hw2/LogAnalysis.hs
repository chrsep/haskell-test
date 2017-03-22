{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage input = go (words input)
        where
         go ("I":time:string) =
          LogMessage Info (read time) (unwords string)
         go ("E":err:time:string) =
          LogMessage (Error (read err)) (read time) (unwords string)
         go ("W":time:string) =
          LogMessage Warning (read time) (unwords string)
         go _ = Unknown input

parse :: String -> [LogMessage]
parse input = go (lines input)
        where
         go (errLog:errLogs) = ((parseMessage errLog):(parse (unlines errLogs)))
{-
insert :: LogMessage -> MessageTree -> MessageTree

build :: [LogMessage] -> MessageTree

inOrder :: MessageTree -> [LogMessage]

-}
