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
    T.interact (T.pack . process . parseEdges)
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

removeNode :: Ord a => a -> Graph a -> (S.Set a, Graph a)
removeNode a graph@(Graph g) = let
    Node _ parents children = M.findWithDefault (newNode a) a g

    makeParent x = Edge x a
    makeChild x = Edge a x

    edges = (makeParent <$> toList parents) ++ (makeChild <$> toList children)

    go (ready, graph1) e@(Edge _ child)
        | canStart (findNode child graph') = (S.insert child ready, graph')
        | otherwise = (ready, graph')
        where graph' = removeEdge e graph1
    
    in foldl' go (S.empty, graph) edges

removeNodes :: Ord a => [a] -> Graph a -> (S.Set a, Graph a)
removeNodes nodes graph = foldl' go (S.empty, graph) nodes where
    go (ready, g) node = (S.union newReady ready, g') where
        (newReady, g') = removeNode node g

topoSort :: (Ord a) => Graph a -> [a]
topoSort graph@(Graph internal) = let
    go ready g = case S.lookupMin ready of
        Nothing -> []
        Just node -> let
            (newReady, g') = removeNode node g   
            in node : go (S.delete node . S.union newReady $ ready) g'
              
    initialReady = S.fromList . fmap (\(Node a _ _) -> a) . filter canStart . toList $ internal
    in go initialReady graph

nodeTime :: Char -> Int
nodeTime c =  fromEnum c - fromEnum 'A' + 1    



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

-- processing
-- tuple is a size, parent pair
process :: [Edge Char] -> String
process edges = let
    graph = foldl' (flip addEdge) newGraph edges
    in show . topoSort $ graph
    
