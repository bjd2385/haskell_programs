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


--    def partition(collection):
--        if len(collection) == 1:
--            yield [collection]
--            return
--
--        first = collection[0]
--        for smaller in partition(collection[1:]):
--            for n, subset in enumerate(smaller):
--                yield smaller[:n] + [[first] + subset] + smaller[n + 1:]
--            yield [[first]] + smaller


forcePart[Int] -> Int
forcePart x = length $ filter filtF (parts x)
  where
    filtF :: [[Int]] -> Bool
    filtF = or . map (\s -> (0 `elem` s && 1 `elem` s) 
                         || (0 `elem` s && 2 `elem` s)
                         || (0 `elem` s && 1 `elem` s && 2 `elem` s))
    parts :: [Int] -> [[[Int]]]
    parts [s] = [[[s]]]
    parts (s:ss) = [ | 
                       take y part ++ [[s] ++ subset] ++ drop (y + 1) part, 
                       (y,subset) <- zip part [0..(length part)], 
                       smaller <- parts ss ]


show' :: Show a => a -> IO ()
show' = putStrLn . show


main :: IO ()
main = (show' $ (2 * bell 9 - bell 8)) >>= (\() -> show' $ 2 * forcePart [0..8] - forcePart [0..7])
