module Utils
  ( modifyAt,
  )
where

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt i f ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (x : xs) = f x : xs
    go n (x : xs) = x : go (n -1) xs
    go _ [] = []
