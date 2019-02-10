import Prelude hiding (Left, Right)

import Data.Array (Array, array, (!))

import Control.Monad.RWS (RWS, evalRWS, put, get, reader, writer)

import Data.Maybe (catMaybes)
import Data.List (sort)
import Control.Applicative (liftA2)

main :: IO ()
main = do
    interact (process . parseFile)
    putStrLn ""

process :: (Tracks, [Cart]) -> String
--process (ts, cs) = unlines . fmap show . take 20 . iterate (stepCarts ts) $ cs
process (ts, cs) = let
    (cart, collisions) = evalRWS steps ts (CartList (cs, []))
    in (unlines . fmap show $ collisions) ++ show cart


-- track type
data Track = Horizontal | Vertical | Backslash | Slash | Intersection | Empty
newtype Tracks = Tracks (Array (Int, Int) Track)
data Intention = GoingLeft | GoingStraight | GoingRight deriving (Show)
data Direction = Up | Down | Left | Right deriving (Show)
data Cart = Cart Int Int Intention Direction deriving (Show)
newtype Collision = Collision (Int, Int) deriving (Show)
newtype CartList = CartList ([Cart], [Cart]) deriving (Show)
type TrackState = RWS Tracks [Collision] CartList

instance Eq Cart where
    (==) (Cart x1 y1 _ _) (Cart x2 y2 _ _)
        | x1 == x2 && y1 == y2 = True
        | otherwise = False
instance Ord Cart where
    compare (Cart x1 y1 _ _) (Cart x2 y2 _ _) =
        compare y1 y2 <> compare x1 x2

parseFile :: String -> (Tracks, [Cart])
parseFile text = let
    ls = lines text

    width = length . head $ ls
    height = length ls

    getXY offset = let (y, x) = offset `quotRem` width in (x, y)
    coords = fmap getXY [(0 :: Int)..]

    cells = fmap parseCell . zip coords . concat $ ls

    tracks = Tracks . array ((0, 0), (width, height)) . zip coords . fmap fst $ cells
    carts = catMaybes . fmap snd $ cells

    in (tracks, carts)

parseCell :: ((Int, Int), Char) -> (Track, Maybe Cart)
parseCell ((x, y), char) = case char of
    ' ' -> (Empty, Nothing)
    '|' -> (Vertical, Nothing)
    '-' -> (Horizontal, Nothing)
    '+' -> (Intersection, Nothing)
    '/' -> (Slash, Nothing)
    '\\' -> (Backslash, Nothing)
    '<' -> (Horizontal, Just (Cart x y GoingLeft Left))
    '>' -> (Horizontal, Just (Cart x y GoingLeft Right))
    '^' -> (Vertical, Just (Cart x y GoingLeft Up))
    'v' -> (Vertical, Just (Cart x y GoingLeft Down))
    _ -> error "parseCell: invalid character"

updateCart :: Cart -> TrackState Cart
updateCart (Cart x y intent dir) = reader (\(Tracks arr) -> case arr ! (x, y) of
    Horizontal -> case dir of
        Left -> Cart (x - 1) y intent Left
        Right -> Cart (x + 1) y intent Right
        _ -> error "updateCart: Cart going up/down on left/right trac.k"
    Vertical -> case dir of
        Up -> Cart x (y - 1) intent Up
        Down -> Cart x (y + 1) intent Down
        _ -> error "updateCart: Cart going left/right on up/down track."
    Backslash -> case dir of
        Up -> Cart (x - 1) y intent Left
        Down -> Cart (x + 1) y intent Right
        Left -> Cart x (y - 1) intent Up
        Right -> Cart x (y + 1) intent Down
    Slash -> case dir of 
        Up -> Cart (x + 1) y intent Right
        Down -> Cart (x - 1) y intent Left
        Left -> Cart x (y + 1) intent Down
        Right -> Cart x (y - 1) intent Up
    Intersection -> case dir of
        Up -> case intent of
            GoingLeft -> Cart (x - 1) y GoingStraight Left
            GoingStraight -> Cart x (y - 1) GoingRight Up
            GoingRight -> Cart (x + 1) y GoingLeft Right
        Down -> case intent of
            GoingLeft -> Cart (x + 1) y GoingStraight Right
            GoingStraight -> Cart x (y + 1) GoingRight Down
            GoingRight -> Cart (x - 1) y GoingLeft Left
        Left -> case intent of
            GoingLeft -> Cart x (y + 1) GoingStraight Down
            GoingStraight -> Cart (x - 1) y GoingRight Left
            GoingRight -> Cart x (y - 1) GoingLeft Up
        Right -> case intent of 
            GoingLeft -> Cart x (y - 1) GoingStraight Up
            GoingStraight -> Cart (x + 1) y GoingRight Right
            GoingRight -> Cart x (y + 1) GoingLeft Down
    Empty -> error "updateCart: Cart on empty square")

steps :: TrackState Cart
steps = do
    CartList (as, bs) <- get
    case (as, bs) of
        ([], []) -> error "steps: No last cart"
        (c: [], []) -> return c
        _ -> step *> steps

step :: TrackState ()
step = do
    CartList (checkedCs, uncheckedCs) <- get
    case uncheckedCs of
        [] -> put $ CartList ([], sort checkedCs)
        (c: cs) -> do
            c' <- updateCart c
            (collisions1, checkedCs') <- testCollisions c' checkedCs
            (collisions2, cs') <- testCollisions c' cs
            let newCheckedCs = if null collisions2 && null collisions1
                then (c': checkedCs')
                else checkedCs'
            put $ CartList (newCheckedCs, cs')
            
testCollision :: Cart -> Cart -> TrackState Bool
testCollision c1@(Cart x y _ _) c2
    | c1 == c2 = writer (True, [Collision (x, y)])
    | otherwise = return False

testCollisions :: Cart -> [Cart] -> TrackState ([Cart], [Cart])
testCollisions c cs = breakM (testCollision c) $ cs

-- generalisation od Data.List.break
{-# INLINE breakM #-}
breakM :: (Applicative m) => (a -> m Bool) -> [a] -> m ([a], [a])
breakM predicate = foldr go (pure ([], [])) where
    addSatisfied x (ss, us) = (x: ss, us)
    addUnSat x (ss, us) = (ss, x: us)
    go x = liftA2 (\flag -> if flag then addSatisfied x else addUnSat x) (predicate x)
