module Hw4 where
import           Data.List ((\\))

-- Ex 1
fun1 :: [Integer] ->  Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) *  fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] ->  Integer
fun1' = product . map (-2+) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)


-- Ex 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

{- First failed implementation -}
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert :: b -> Tree b -> Tree b
    insert x (Node h lt@(Node l _ _ _) v rt@(Node r _ _ _))
      | l > r = Node (h+1) lt v (insert x rt)
      | otherwise = Node h (insert x lt) v rt
    insert x (Node h Leaf v Leaf) = Node (h + 1) Leaf v (insert x Leaf)
    insert x (Node h l@Leaf v r) = Node h (insert x l) v r
    insert x (Node h l v r@Leaf) = Node h l v (insert x r)
    insert x Leaf = Node 0 Leaf x Leaf

{- Second Implementation -}
foldTree'' :: [a] -> Tree a
foldTree'' = foldr insert Leaf
  where
    insert :: b -> Tree b -> Tree b
    insert x (Node depth left val right)
      | treeDepth left > treeDepth right = Node depth left val toRight
      | treeDepth left < treeDepth right = Node depth toLeft val right
      | treeDepth toLeft > treeDepth toRight = Node depth left val toRight
      | otherwise = Node (1 + treeDepth toLeft) toLeft val right
      where
        toLeft = insert x left
        toRight = insert x right
    insert x Leaf = Node 0 Leaf x Leaf

treeDepth :: Tree a -> Integer
treeDepth (Node x _ _ _) = x
treeDepth Leaf           = 0

-- Ex 3

xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\y h w -> h (f w y)) id xs base

-- Ex 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  takeWhile (<= 2 * n + 2) .
  map (\x -> (2 * x) + 1) .
  (\\) [1 ..] . map (\(i, j) -> i + j + (2 * i * j)) . filter (uncurry (>=)) $
  cartProd [1 .. n] [1 .. n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys =[(x, y) | x <- xs, y <- ys]

