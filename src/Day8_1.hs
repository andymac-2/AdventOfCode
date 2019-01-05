import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec (Parsec, parse, count, errorBundlePretty)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void (Void)

import Control.Lens ((^?), ix)

main :: IO ()
main = do
    T.interact (T.pack . show . value . parseTree)
    putStrLn ""

-- Tree class
data LicenseTree a = LicenseTree
    { ltChildren :: [LicenseTree a]
    , ltMetadata :: [a]
    }

instance Functor LicenseTree where
    fmap f (LicenseTree children metadata) = let
        metadata' = fmap f metadata
        children' = fmap (fmap f) children
        in LicenseTree children' metadata'

instance Foldable LicenseTree where
    foldr f s (LicenseTree children metadata) = let
        s' = foldr f s metadata
        in foldr (flip (foldr f)) s' children

value :: LicenseTree Int -> Int
value (LicenseTree [] ms) = sum ms
value (LicenseTree cs ms) = let
    -- children are 1 indexed.
    childValue index = case cs ^? ix (index - 1) of
        Just child -> value child
        Nothing -> 0
    in sum . fmap childValue $ ms

-- parse input
type Parser = Parsec Void T.Text

parseTree :: T.Text -> LicenseTree Int
parseTree str = case parse treeP "" str of
    Left err -> error (errorBundlePretty err)
    Right val -> val

treeP :: Parser (LicenseTree Int)
treeP = do
    nChildren <- integerP
    nMetadata <- integerP
    LicenseTree <$> count nChildren treeP <*> count nMetadata integerP

-- optionally signed integer
integerP :: Parser Int
integerP = L.signed space L.decimal <* space
