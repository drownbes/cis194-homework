module Homework4 where

import Data.Monoid

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1_new :: [Integer] -> Integer
fun1_new = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


fun2_new :: Integer -> Integer
fun2_new = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n
                                                                  then n `div` 2
                                                                  else 3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node height _ _ _) = height

foldTree :: (Ord a, Show a) => [a] -> Tree a
foldTree = foldr insert Leaf

insert :: (Ord a, Show a) => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x t@(Node n left x' right)
  | x' < x = rotate $ Node (n+1) left x' (insert x right)
  | x' > x = rotate $ Node (n+1) (insert x left) x' right
  | otherwise = t

getBalance (Node _ left _ right) = getHeight right - getHeight left


rotateLeft Leaf = Leaf
rotateLeft (Node hz t1 vz (Node hy t2 vy (Node hx t3 vx t4))) =
  Node hy (Node (hz-2) t1 vz t2) vy (Node hx t3 vx t4)
rotateLeft (Node hz t1 vz (Node hy t2 vy Leaf)) =
  Node (hy+1) (Node (hz-1) t1 vz t2) vy Leaf


rotateRight Leaf = Leaf
rotateRight (Node hz (Node hy (Node hx t1 vx t2) vy t3) vz t4) =
  Node hy (Node hx t1 vx t2) vy (Node (hz-2) t3 vz t4)
rotateRight (Node hz (Node hy Leaf vy t3) vz t4) =
  Node (hy+1) Leaf vy (Node (hz-1) t3 vz t4)


getRight Leaf = Leaf
getRight (Node _ _ _ r) = r

getLeft Leaf = Leaf
getLeft (Node _ l _ _) = l

getRightHeight = getHeight . getRight

getLeftHeight = getHeight . getLeft


rotate :: (Ord a, Show a) => Tree a -> Tree a
rotate Leaf = Leaf
rotate t@(Node h l v r)
  | b == -2 = if getLeftHeight l  >= getRightHeight l then rotateRight t  else rotateRight (Node h (rotateLeft l) v r)
  | b ==  2 = if getRightHeight r >= getLeftHeight r  then rotateLeft t   else rotateLeft (Node h l v (rotateRight r))
  | otherwise = t
  where b = getBalance t

draw
  :: Show a
  => Tree a -> String
draw t = '\n' : draw_ t 0 <> "\n"
  where
    draw_ Leaf _ = []
    draw_ (Node h l v r) d = draw_ r (d + 1) <> node <> draw_ l (d + 1)
      where
        node = padding d <> show (v) <> "\n"
        padding n = replicate (n * 4) ' '


xor :: [Bool] -> Bool
xor xs = odd $ foldr (\x acc -> if x then (acc + 1) else acc) 0 xs

xor' :: [Bool] -> Bool
xor' = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f(x):acc) []


