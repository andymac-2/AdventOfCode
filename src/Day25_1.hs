import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec (Parsec, parse, sepEndBy)
import Text.Megaparsec.Char (space, newline, char)

import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Map.Strict as M

import Data.Foldable (foldl', toList)
import Data.Either (isLeft)

main :: IO ()
main = do
    T.interact (T.pack . process . parseNodes)
    putStrLn ""

process :: [Node] -> String
process = show . getRoots . djsFromList hasEdge

data Node = Node Int Int Int Int deriving (Eq, Ord, Show)

-- parse input
type Parser = Parsec () T.Text

parseNodes :: T.Text -> [Node]
parseNodes str = case parse nodeListP "" str of
    Left _ -> error "Invalid Parse"
    Right nodes -> nodes

nodeListP :: Parser [Node]
nodeListP = sepEndBy nodeP newline

nodeP :: Parser Node
nodeP = Node
    <$> integerP
    <* char ','
    <*> integerP
    <* char ','
    <*> integerP
    <* char ','
    <*> integerP

integerP :: Parser Int
integerP = L.signed space L.decimal

-- processing
-- tuple is a size, parent pair
type Djs a = M.Map a (Either Int a)

djsFromList :: Ord a => (a -> a -> Bool) -> [a] -> Djs a
djsFromList f list = let
    disconnected = M.fromList . fmap (\x -> (x, Left 1)) $ list

    pairs = (\x y -> (x, y)) <$> list <*> list

    go djs (x, y)
        | f x y = unionDjs x y djs
        | otherwise = djs

    in foldl' go disconnected pairs

getRoots :: Djs a -> Int
getRoots = length . filter isLeft . toList

isRoot :: Ord a => a -> Djs a -> Bool
isRoot node djs = case M.lookup node djs of
    Just (Left _) -> True
    Just (Right _) -> False
    Nothing -> error "isRoot: attempted to lookup node not in set"

findRoot :: Ord a => a -> Djs a -> ((a, Int), Djs a)
findRoot node djs = case M.lookup node djs of
    Just (Left size) -> ((node, size), djs)
    Just (Right parent) ->  let
        ((root, size), flatDjs) = findRoot parent djs
        in ((root, size), setNode node (Right root) flatDjs)
    Nothing -> error "findRoot: attempted to lookup node not in set"

setNode :: Ord a => a -> Either Int a -> Djs a -> Djs a
setNode = M.insert

unionDjs :: Ord a => a -> a -> Djs a -> Djs a
unionDjs x y djs
    | rootx == rooty = djs'
    | sizex < sizey 
        = setNode rooty (Left $ sizex + sizey) . setNode rootx (Right rooty) $ djs'
    | otherwise
        = setNode rootx (Left $ sizex + sizey) . setNode rooty (Right rootx) $ djs'
    where 
        ((rootx, sizex), djs1) = findRoot x djs
        ((rooty, sizey), djs') = findRoot y djs1

-- manhattan distance equals 3
hasEdge :: Node -> Node -> Bool
hasEdge (Node x1 x2 x3 x4) (Node y1 y2 y3 y4)
    = abs (x1 - y1) + abs (x2 - y2) + abs (x3 - y3) + abs (x4 - y4) <= 3
