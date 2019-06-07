import           Data.Char

main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
  let biggerBetter = map toUpper name
      worse = map toLower biggerBetter
  putStrLn $ "hey," ++ biggerBetter ++ worse
