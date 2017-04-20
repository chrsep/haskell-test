module Golf where

{-
This function will map a lambda function through a list of integer
that have the length of the input.

The lambda will filter the input according to the integer n (from
the list [1..length l]) using list comprehension. The list
comprehension will return x, where x index mod by the input to the
lambda equals to 0.
-}
skips :: [a] -> [[a]]
skips l = map (\n -> [x | (x, y) <- zip l [1 ..], mod y n == 0]) [1 .. length l]

{-
This function will pattern match with the first three element of the
input list. It will then checks if the middle element is the biggest
using list comprehension, returning the middle element if true, or an
empty list otherwise.

It then proceeds to recursively call itself and add the resulting return
value to together.
-}
localMaxima :: [Integer] -> [Integer]
localMaxima ( x:y:zs@(z:_) ) = [y | x < y && z < y] ++ localMaxima zs
localMaxima _                = []

{-
histogram
This function first creates a list of number containing the amount of
occurrences of every number in the input list (the amt list).

It then maps through a list containing the number from the highest amount
of occurrences to 0 with a function that maps through the amt list, this inner
map function will return list of spaces and *, it decides whether to return
spaces or star by comparing each amount of occurrence in amt to the list
passed to the upper map function.
-}
histogram :: [Integer] -> String
histogram l = unwords (map line [mx,( mx - 1 ) .. 1]) ++ "\n==========\n0123456789\n"
  where
    cntr n acc x
      | x == n = acc + 1
      | otherwise = acc
    amt = map (\x -> foldl (cntr x) 0 l) [0 .. 9]
    mx = maximum amt
    xorstar x y = if y >= x then '*' else ' '
    line x = "\n" ++ map ( xorstar x ) amt
