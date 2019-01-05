main :: IO ()
main = interact (show . sum . fmap parseInt . lines)

parseInt :: String -> Int
parseInt (x: xs) = let
    absolute = read xs
    in case x of
        '+' -> absolute
        '-' -> -absolute
