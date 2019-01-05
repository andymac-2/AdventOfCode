import Data.List

main :: IO ()
main = interact (uncurry diff . head . filter differBy1 . pairs . lines)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs t@(x: xs) = fmap (\y -> (x, y)) t ++ pairs xs

differBy1 :: Eq a => ([a], [a]) -> Bool
differBy1 (xs, ys) = differByN 1 xs ys

differByN :: Eq a => Int -> [a] -> [a] -> Bool
differByN 0 [] [] = True
differByN _ [] [] = False
differByN n (x: xs) (y: ys)
    | x == y = differByN n xs ys
    | n > 0 = differByN (n - 1) xs ys
    | otherwise = False

diff :: Eq a => [a] -> [a] -> [a]
diff [] [] = []
diff (x: xs) (y: ys)
    | x == y = x: diff xs ys
    | otherwise = diff xs ys
