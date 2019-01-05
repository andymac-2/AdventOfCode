import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec (Parsec, parse, sepEndBy)
import Text.Megaparsec.Char (newline, char, space)
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Foldable (foldl', minimumBy)
import Data.Ord (comparing)
import Data.List (sortOn)

maxDistance = 10000

main :: IO ()
main = do
    -- part 1
    -- T.interact (\t -> let
    --    points = parsePoints t
    --    cellSizes = getCellSizes points . removeInfinities $ points
    --    in T.pack . show . maximum $ cellSizes
    --    )

    T.interact (\t -> let
        points = parsePoints t
        set = fillWith (median points) (\p -> intrinsic points p < maxDistance)
        in T.pack . show . length $ set
        )
    putStrLn ""

-- Point class
data Point a = Point 
    { px :: a
    , py :: a
    } deriving (Show, Eq, Ord)

manhattan :: Num a => Point a -> Point a -> a
manhattan (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

intrinsic :: Num a => [Point a] -> Point a -> a
intrinsic ps p = sum . fmap (manhattan p) $ ps

median :: Integral a => [Point a] -> Point a
median ps = Point x y where
    halflen = (length ps) `quot` 2
    x = fmap px ps !! halflen
    y = fmap py ps !! halflen

-- Bounding box class
data BBox a = BBox
    { bboxTop :: a
    , bboxBotttom :: a
    , bboxLeft :: a
    , bboxRight :: a
    } deriving (Show)

getBounds :: Ord a => [Point a] -> BBox a
getBounds points = BBox top bot left right where
    xs = (\(Point x _) -> x) <$> points
    ys = (\(Point y _) -> y) <$> points

    top = minimum xs
    bot = maximum xs
    left = minimum ys
    right = maximum ys

outerEdges :: Integral a => BBox a -> [Point a]
outerEdges (BBox t b l r) = tEdge ++ rEdge ++ bEdge ++ lEdge where
    tEdge = (flip Point) t <$> [l .. r]
    rEdge = Point r <$> [t + 1 .. b - 1]
    bEdge = (flip Point) b <$> [l .. r]
    lEdge = Point l <$> [t + 1 .. b - 1]

everyPoint :: Integral a => BBox a -> [Point a]
everyPoint (BBox t b l r) = Point <$> [l .. r] <*> [t .. b]

-- flood fill
fillWith :: Point Int -> (Point Int -> Bool) -> S.Set (Point Int)
fillWith start f = go [start] S.empty where
    go [] set = set
    go (p@(Point x y): stack) set
        | not (f p) || S.member p set = go stack set
        | otherwise = go (adjacents ++ stack) set'
        where
            set' = S.insert p set
            adjacents = 
                [ Point (x + 1) y
                , Point (x - 1) y
                , Point x (y + 1)
                , Point x (y - 1)
                ]

-- voronoi functions
closestPoint :: (Num a, Ord a) => [Point a] -> Point a -> Point a
closestPoint ps p = minimumBy (comparing (manhattan p)) ps

removeInfinities :: Integral a => [Point a] -> M.Map (Point a) Int
removeInfinities ps = foldr M.delete mapAll closestToEdges where
    bbox = getBounds ps
    mapAll = M.fromList . fmap (\p -> (p, 0)) $ ps
    closestToEdges = fmap (closestPoint ps) . outerEdges $ bbox  
    
getCellSizes :: (Integral a, Ord a) => [Point a] -> M.Map (Point a) Int ->  M.Map (Point a) Int
getCellSizes ps cellMap = let
    bbox = getBounds ps
    in foldl' (flip $ M.adjust (1+)) cellMap 
        . fmap (closestPoint ps) 
        . everyPoint 
        $ bbox   

-- parse input
type Parser = Parsec () T.Text

parsePoints :: T.Text -> [Point Int]
parsePoints str = case parse pointListP "" str of
    Left _ -> error "Invalid Parse"
    Right nodes -> nodes

pointListP :: Parser [Point Int]
pointListP = sepEndBy pointP newline

-- parent must be finished before child can begin.
pointP :: Parser (Point Int)
pointP = Point
    <$> integerP
    <* char ','
    <* space
    <*> integerP

integerP :: Parser Int
integerP = L.signed space L.decimal
