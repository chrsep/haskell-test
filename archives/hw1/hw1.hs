{-# OPTIONS_GHC -Wall #-}

doubler :: [Integer]->[Integer]
doubler [] = []
doubler (x:xs)
  | length xs `mod` 2 /= 1 = x : doubler xs
  | otherwise = x*2 : doubler xs

sum'em :: [Integer]->Integer
sum'em x = go (doubler x)
        where
         go (y:ys) = combine y + go ys
         go [] = 0

combine :: Integer->Integer
combine x
  | x < 10 = x
  | otherwise = x `div` 10 + x `mod` 10

validate::[Integer]->Bool
validate x = (sum'em x `mod` 10) == 0



{- Excercise 2 -}

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
