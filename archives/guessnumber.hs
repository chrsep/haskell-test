import System.Random
import Control.Monad(when)

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber::StdGen -> IO ()
askForNumber g = do
  let (randomNumber, randomGen) = randomR (1,10) g::(Int, StdGen)
  putStr "Guess what number i am thinking of: "
  numberChosen <- getLine
  when(not $ null numberChosen) $ do
    let number = read numberChosen
    if randomNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randomNumber
    askForNumber randomGen
