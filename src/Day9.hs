import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

nPlayers :: Int
nPlayers = 463

nMarbles :: Int
nMarbles = 7178700

main :: IO ()
main = do
    putStrLn . show . maxScore . (!! nMarbles) . iterate update . newGame $ nPlayers
    putStrLn ""

-- marblecircle class
data MarbleCircle = MC [Int] [Int]

instance Show MarbleCircle where
    show (MC xs ys) = let
        xs' = unwords . fmap show . reverse $ xs
        ys' = unwords . fmap show $ ys
        in ys' ++ " " ++ xs'

newMarbleCircle :: MarbleCircle
newMarbleCircle = MC [0] []

rotR :: MarbleCircle -> MarbleCircle
rotR t@(MC [] []) = t
rotR (MC xs []) = rotR (MC [] (reverse xs))
rotR (MC xs (y: ys)) = MC (y :xs) ys

rotL :: MarbleCircle -> MarbleCircle
rotL t@(MC [] []) = t
rotL (MC [] ys) = rotL (MC (reverse ys) [])
rotL (MC (x: xs) ys) = MC xs (x: ys)

rotNL :: Int -> MarbleCircle -> MarbleCircle
rotNL n = (!! n) . iterate rotL

insert :: Int -> MarbleCircle -> MarbleCircle
insert e (MC xs ys) = MC (e: xs) (ys)

remove :: MarbleCircle -> (Int , MarbleCircle) 
remove (MC (x: xs) ys) = (x, MC xs ys)
remove (MC [] []) = error "remove: empty MarbleCircle"
remove (MC [] ys) = remove (MC (reverse ys) [])

-- game class
data Game = Game 
    Int -- current marble
    Int -- number of platers
    Int -- current player
    (M.Map Int Int) -- player scores
    MarbleCircle -- circle
    deriving (Show)

maxScore :: Game -> Int
maxScore (Game _ _ _ m _) = maximum m

maxElf :: Game -> (Int, Int)
maxElf (Game _ _ _ s _) = M.foldrWithKey go (0, 0) s where
    go elfId score t@(currentMax, _)
        | score > currentMax = (score, elfId)
        | otherwise = t

getElfScore :: Int -> Game -> Int
getElfScore n (Game _ _ _ s _) = fromMaybe 0 . M.lookup n $ s

newGame :: Int -> Game
newGame n = Game 1 n 0 M.empty newMarbleCircle

update :: Game -> Game
update (Game m n p s c)
    | m `rem` 23 /= 0 = Game m' n p' s cNormal
    | otherwise = Game m' n p' s' cWeird
    where
        m' = m + 1
        p' = (p + 1) `rem` n
        cNormal = insert m . rotR $ c
        (current, c') = remove . rotNL 7 $ c
        s' = M.insertWith (+) p (current + m) s
        cWeird = rotR c'
