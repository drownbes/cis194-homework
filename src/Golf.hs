module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips xs = map (uncurry every) ixs
  where
    ixs = zip [1 .. (length xs)] (repeat xs)
    every n =
      map head . takeWhile (not . null) . iterate (drop n) . drop (n - 1)

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:other)
  | y > x && y > z = y : localMaxima (z:other)
  | otherwise = localMaxima (y:z:other)
localMaxima (x:y) = []
localMaxima [] = []
