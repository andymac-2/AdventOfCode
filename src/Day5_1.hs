import Data.Char (toLower, isLower, isLetter)

main :: IO ()
main = do
    interact smallestPoly
    putStrLn ""

smallestPoly :: String -> String
smallestPoly str = let
    unredux = (\x -> toPolymerWoUnit x str) <$> "abcdefghijklmnopqrstuvwxyz_"
    lengths = length . reducePolymer <$> unredux
    in show lengths

type Polymer = ([Char], [Char])

toPolymerWoUnit :: Char -> String -> Polymer
toPolymerWoUnit c = let
    test a = c /= toLower a
    in toPolymer . filter test

toPolymer :: String -> Polymer
toPolymer str = ([], filter isLetter str)

oppositeCase :: Char -> Char -> Bool
oppositeCase x y = isLower x /= isLower y && toLower x == toLower y

reducePolymer :: Polymer -> String
reducePolymer (xs, []) = reverse xs
reducePolymer (xs, [y]) = reducePolymer ((y: xs), [])
reducePolymer ([], (y1: y2: ys)) = case oppositeCase y1 y2 of
    False -> reducePolymer ([y1], (y2: ys))
    True -> reducePolymer ([], ys)
reducePolymer ((x: xs), (y1: y2: ys)) = case oppositeCase y1 y2 of
    False -> reducePolymer ((y1: x: xs), (y2: ys))
    True -> reducePolymer (xs, (x: ys))
