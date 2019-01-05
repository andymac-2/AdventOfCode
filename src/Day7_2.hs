import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec (Parsec, parse, sepEndBy, anySingle)
import Text.Megaparsec.Char (newline, string)

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.Foldable (foldl', toList)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    T.interact (T.pack . nWorkers 5 . fromEdges . parseEdges)
    putStrLn ""

-- Node class
-- Node nodeId parents children
data Node a = Node a (S.Set a) (S.Set a) deriving (Eq, Ord, Show)

newNode :: a -> Node a
newNode a = Node a S.empty S.empty

canStart :: Node a -> Bool
canStart (Node _ parents _) = S.null parents

addChild :: Ord a => a -> Node a -> Node a
addChild c (Node a parents children) =
    Node a parents (S.insert c children)

addParent :: Ord a => a -> Node a -> Node a
addParent p (Node a parents children) =
    Node a (S.insert p parents) children

removeChild :: Ord a => a -> Node a -> Node a
removeChild c (Node a parents children) =
    Node a parents (S.delete c children)

removeParent :: Ord a => a -> Node a -> Node a
removeParent p (Node a parents children) =
    Node a (S.delete p parents) children

-- Edge class
-- Edge parent child
data Edge a = Edge a a deriving (Show)

-- graph class
newtype Graph a = Graph (M.Map a (Node a)) deriving (Show)

newGraph :: Graph a
newGraph = Graph M.empty

fromEdges :: Ord a => [Edge a] -> Graph a
fromEdges = foldl' (flip addEdge) newGraph

findNode :: Ord a => a -> Graph a -> Node a
findNode a (Graph g) = g M.! a

addEdge :: Ord a => Edge a -> Graph a -> Graph a
addEdge (Edge parent child) (Graph graph) = let
    graph' = M.alter (Just . addParent parent . fromMaybe (newNode child)) child graph
    graph'' = M.alter (Just . addChild child . fromMaybe (newNode parent)) parent graph'
    in Graph graph''

removeEdge :: Ord a => Edge a -> Graph a -> Graph a
removeEdge (Edge parent child) (Graph graph) = let
    graph' = M.adjust (removeParent parent) child graph
    graph'' = M.adjust (removeChild child) parent graph'
    in Graph graph''

removeNode :: Ord a => a -> Graph a -> Graph a
removeNode a (Graph g) = let
    Node _ parents children = M.findWithDefault (newNode a) a g
    g' = M.delete a g

    makeParent x = Edge x a
    makeChild x = Edge a x

    edges = (makeParent <$> toList parents) ++ (makeChild <$> toList children)

    in foldr removeEdge (Graph g') edges


-- special graph functions
type Snapshot a = (M.Map a Int, Graph a)

getReadys :: Graph Char -> M.Map Char Int
getReadys (Graph g) 
    = M.fromList . fmap (\(Node a _ _) -> (a, nodeTime a)) . filter canStart . toList $ g

workOnNode :: Char -> Snapshot Char -> Snapshot Char
workOnNode nodeid (readys, graph) = (readys'', graph') where
    readys' = M.alter (\x -> case x of
        Just time
            | time - 1 <= 0 -> Nothing
            | otherwise -> Just (time - 1)
        Nothing -> error "workOnNode: node not in ready list"
        ) nodeid readys
    graph' = case M.lookup nodeid readys' of
        Nothing -> removeNode nodeid graph
        Just _ -> graph
    readys'' = M.union readys' (getReadys graph')

workOnNNodes :: Int -> Snapshot Char -> Maybe (Snapshot Char)
workOnNNodes n s@(ready, _) = case nodes of
    [] -> Nothing 
    _ -> Just . foldr workOnNode s $ nodes 
    where nodes = take n . M.keys $ ready

build :: (a -> Maybe a) -> a -> [a]
build f state = case f state of
    Nothing -> []
    Just state' -> state : build f state'

nWorkers :: Int -> Graph Char -> String
nWorkers n graph = let
    initialReady = getReadys graph
    in show . length . build (workOnNNodes n) $ (initialReady, graph)

nodeTime :: Char -> Int
nodeTime c =  fromEnum c - fromEnum 'A' + 61

-- parse input
type Parser = Parsec () T.Text

parseEdges :: T.Text -> [Edge Char]
parseEdges str = case parse edgeListP "" str of
    Left _ -> error "Invalid Parse"
    Right nodes -> nodes

edgeListP :: Parser [Edge Char]
edgeListP = sepEndBy edgeP newline

-- parent must be finished before child can begin.
edgeP :: Parser (Edge Char)
edgeP = Edge
    <$ string (T.pack "Step ")
    <*> anySingle
    <* string (T.pack " must be finished before step ") 
    <*> anySingle 
    <* string (T.pack " can begin.")    
