-- I was going to try using Math.Combinat.Partitions.Integer._partition, but
-- this function is of type (Int -> [[Int]], which isn't quite the type I'm
-- looking for in `parts`).


factorial :: Int -> Int
factorial x = if x > 0 then x * (factorial $ x - 1)
              else 1


binomial :: Int -> Int -> Int
binomial n r = (factorial n) `quot` (factorial (n - r) * factorial r)


bell :: Int -> Int
bell n = if n <= 1 then 1
         else sum [binomial (n - 1) k * bell k | k <- [0..(n - 1)]]


forcePart :: [Int] -> Int
forcePart x = length $ filter filtF (partitions x)
  where
    filtF :: [[Int]] -> Bool
    filtF = or . map (\s -> (0 `elem` s && 1 `elem` s && 2 `elem` s)
                         || (0 `elem` s && 1 `elem` s) 
                         || (0 `elem` s && 2 `elem` s))
    -- This is hard!
    -- https://stackoverflow.com/a/46596325/3928184 
    partitions :: [a] -> [[[a]]]
    partitions [] = [[]]
    partitions (x:xs) = expand x $ partitions xs 
      where
        expand :: a -> [[[a]]] -> [[[a]]]
        expand x ys = concatMap (extend x) ys

        extend :: a -> [[a]] -> [[[a]]]
        extend x [] = [[[x]]]
        extend x (y:ys) = ((x:y):ys) : map (y:) (extend x ys)


show' :: Show a => a -> IO ()
show' = putStrLn . show


main :: IO ()
main = (show' $ (2 * bell 9 - bell 8)) >>= (\() -> show' $ 2 * forcePart [0..8] - forcePart [0..7])
