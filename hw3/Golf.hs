module Golf where

skips :: [a] -> [[a]]
skips l = map (map snd . filterElem) [1..(length l)]
  where l' = zip [1..] l
        filterElem n = filter (\x -> fst x `mod` n == 0) l'
