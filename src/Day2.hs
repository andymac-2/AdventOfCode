import Data.Set as S

main :: IO ()
main = interact (show . firstDup . scanl (+) 0 . cycle . fmap parseInt . lines)

firstDup :: [Int] -> Int
firstDup xs = go S.empty xs where
    go elems (x: xs) = case x `member` elems of
        True -> x
        False -> go (S.insert x elems) xs

parseInt :: String -> Int
parseInt (x: xs) = let
    absolute = read xs
    in case x of
        '+' -> absolute
        '-' -> -absolute
