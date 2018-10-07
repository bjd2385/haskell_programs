-- Generate all of the partitions of a list.


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


partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = expand x $ partitions xs
  where
    expand :: a -> [[[a]]] -> [[[a]]]
    expand x ys = concatMap (extend x) ys

    extend :: a -> [[a]] -> [[[a]]]
    extend x [] = [[[x]]]
    extend x (y:ys) = ((x:y):ys) : map (y:) (extend x ys)


main :: IO ()
main = do
  putStrLn . show $ partitions [1..3]
