import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec (Parsec, parse, errorBundlePretty, (<|>), many, eof)
import Text.Megaparsec.Char (space, char, string)

import Data.Void (Void)

import Data.Maybe (catMaybes)
import Data.Foldable (foldl')

main :: IO ()
main = do
    T.interact (T.pack . show . process . parseFile)
    putStrLn ""

process :: (Gen, [Rule]) -> Int
process (gen, rs) = value . (!! 101) . generations rs $ gen

--process :: (Gen, [Rule]) -> String
--process (gen, rs) = unlines . fmap show . take 100 . generations rs $ gen


-- plant class
data Plant = Plant | NoPlant deriving (Eq)

instance Show Plant where
    show Plant = "#"
    show NoPlant = "."

-- Rules class
type Rule = [Plant]

-- Generation class
data Gen = Gen Int [Plant]

instance Show Gen where
    show (Gen n ps) = "Gen " ++ show n ++ " " ++ plants where
        plants = concat . fmap show $ ps

newGen :: Int -> [Plant] -> Gen
newGen start xs = Gen newStart xs' where
    prefix = replicate 4 NoPlant

    stripEmpty = dropWhile (== NoPlant)
    stripped = reverse . stripEmpty . reverse . stripEmpty $ xs

    xs' = prefix ++ stripped ++ prefix
    newStart = start - 4 + (length . takeWhile (== NoPlant) $ xs)

nextGen :: [Rule] -> Gen -> Gen
nextGen rs (Gen n plants) = let
    grow [_, _, _, _] = []
    grow t@(_: ps)
        | (take 5 t) `elem` rs = Plant: grow ps
        | otherwise = NoPlant: grow ps
    in newGen (n + 2) (grow plants)

generations :: [Rule] -> Gen -> [Gen]
generations rs firstGen = iterate (nextGen rs) firstGen

value :: Gen -> Int
value (Gen n ps) = fst . foldl' go (0, n) $ ps where
    go (acc, cursor) Plant = (acc + cursor, cursor + 1)
    go (acc, cursor) NoPlant = (acc, cursor + 1)

-- parse input
type Parser = Parsec Void T.Text

parseFile :: T.Text -> (Gen, [Rule])
parseFile str = case parse fileP "" str of
    Left err -> error (errorBundlePretty err)
    Right val -> val

fileP :: Parser (Gen, [Rule])
fileP = go
    <$ string (T.pack "initial state: ")
    <*> many plantP
    <* space
    <*> many (ruleP <* space)
    <* eof
    where
    go ps rs = (newGen 0 ps, catMaybes rs)

ruleP :: Parser (Maybe [Plant])
ruleP = go 
    <$> many plantP 
    <* string (T.pack " => ")
    <*> plantP
    where
    go _ NoPlant = Nothing
    go rule Plant = Just rule

plantP :: Parser Plant
plantP = (Plant <$ char '#') 
    <|> (NoPlant <$ char '.')
