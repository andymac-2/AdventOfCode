import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Set as S

import Data.Foldable (foldl')


data BBox = BBox Int Int Int Int deriving (Show, Eq, Ord) -- top bottom left right

data Claim = Claim
    { claimId :: Int
    , claimBounds :: BBox
    } deriving (Show, Eq, Ord) 

main :: IO ()
main = do
    -- part 1
    -- T.interact (T.pack . show . claimTreeOverlap . foldl' (flip insertClaim) emptyClaimTree . parseClaims)
    -- part 2
    T.interact (T.pack . show . process . parseClaims)
    putStrLn ""

process claims = let
    claimtree = foldl' (flip insertClaim) emptyClaimTree claims
    claimquery claim = queryOverlaps (claimBounds claim) claimtree
    claimqueries = claimquery <$> claims
    in filter (\x -> length x == 1) claimqueries


-- parse input
type Parser = Parsec () T.Text

parseClaims :: T.Text -> [Claim]
parseClaims str = case parse claimListP "" str of
    Left _ -> error "Invalid Parse"
    Right claims -> claims

claimListP :: Parser [Claim]
claimListP = sepEndBy claimP newline

claimP :: Parser Claim
claimP = let 
    go claimid left top width height = 
        Claim claimid (BBox top (top + height) left (left + width))
    in go
        <$ char '#'
        <*> integerP
        <* space
        <* char '@'
        <* space
        <*> integerP
        <* char ','
        <*> integerP
        <* char ':'
        <* space
        <*> integerP
        <* char 'x'
        <*> integerP

integerP :: Parser Int
integerP = read <$> some digitChar

checkAllClaimed :: [Claim] -> Int
checkAllClaimed claims = length . filter id $
    cellClaimed claims <$> [0 .. 1100] <*> [0 .. 1100]

cellClaimed :: [Claim] -> Int -> Int -> Bool
cellClaimed claims x y = let
    list = filter (\(Claim _ (BBox t b l r)) -> y >= t && y < b && x >= l && x < r) claims
    in length list > 2

-- process claim tree
data Split = Horizontal Int | Vertical Int deriving (Show)

data SplitCmp = AboveLeft | BelowRight | Bisected

data ClaimTree 
    = Branch BBox Split ClaimTree ClaimTree
    | SingleClaim BBox Claim 
    | MultiClaim BBox [Claim]
    | EmptyTree BBox
    deriving (Show)

emptyClaimTree :: ClaimTree
emptyClaimTree = EmptyTree (BBox 0 2000 0 2000)

-- test for an overlap of two claims. If there is an overlap, return the BBox
-- where the two overlap, otherwise return a Split which separates to two boxes.
overlaps :: BBox -> BBox -> Either Split BBox
overlaps (BBox t1 b1 l1 r1) (BBox t2 b2 l2 r2)
    | t1 >= b2 = Left (Horizontal b2)
    | l1 >= r2 = Left (Vertical r2)
    | t2 >= b1 = Left (Horizontal b1)
    | l2 >= r1 = Left (Vertical r1)
    | otherwise = Right (BBox (max t1 t2) (min b1 b2) (max l1 l2) (min r1 r2))

compareSplit :: Claim -> Split -> SplitCmp
compareSplit (Claim _ (BBox t b _ _)) (Horizontal splitLine)
    | t >= splitLine = BelowRight
    | b <= splitLine = AboveLeft
    | otherwise = Bisected
compareSplit (Claim _ (BBox _ _ l r)) (Vertical splitLine)
    | l >= splitLine = BelowRight
    | r <= splitLine = AboveLeft
    | otherwise = Bisected

splitBBox :: BBox -> Split -> (BBox, BBox)
splitBBox (BBox t b l r) (Horizontal splitline)
    = ((BBox t splitline l r), (BBox splitline b l r))
splitBBox (BBox t b l r) (Vertical splitline)
    = ((BBox t b l splitline), (BBox t b splitline r))

insertClaim :: Claim -> ClaimTree -> ClaimTree
insertClaim claim t@(EmptyTree bbox) = case overlaps (claimBounds claim) bbox of
    Left _ -> t
    Right _ -> SingleClaim bbox claim

insertClaim claim t@(MultiClaim bbox list) = 
    case overlaps (claimBounds claim) bbox of
        Left _ -> t
        Right _ -> MultiClaim bbox (claim: list)

insertClaim claim t@(Branch bbox split aboveLeft belowRight) = let
    newAboveLeft = insertClaim claim aboveLeft
    newBelowRight = insertClaim claim belowRight

    newBranch = case compareSplit claim split of
        AboveLeft -> Branch bbox split newAboveLeft belowRight
        BelowRight -> Branch bbox split aboveLeft newBelowRight
        Bisected -> Branch bbox split newAboveLeft newBelowRight
    in case overlaps (claimBounds claim) bbox of
        Left _ -> t
        Right _ -> newBranch

insertClaim claim1 tree@(SingleClaim bbox@(BBox tb bb lb rb) claim2) = let
    insertClaims = insertClaim claim2 . insertClaim claim1
    bbox1 = claimBounds claim1
    bbox2 = claimBounds claim2

    splitTreeAt split = let
        (al, br) = splitBBox bbox split
        newAboveLeft = insertClaims $ EmptyTree al
        newBelowRight = insertClaims $ EmptyTree br
        in Branch bbox split newAboveLeft newBelowRight

    newTree = case overlaps bbox1 bbox2 of
        Left split -> splitTreeAt split
        Right intersection@(BBox t b l r)
            | t > tb -> splitTreeAt (Horizontal t)
            | b < bb -> splitTreeAt (Horizontal b)
            | l > lb -> splitTreeAt (Vertical l)
            | r < rb -> splitTreeAt (Vertical r)
            | otherwise -> MultiClaim truncatedIntersect [claim1, claim2]
            where truncatedIntersect = fromRight (overlaps intersection bbox)

    in case overlaps bbox1 bbox of
        Left _ -> tree
        Right _ -> newTree

queryOverlaps :: BBox -> ClaimTree -> S.Set Claim
queryOverlaps _ (EmptyTree _) = S.empty
queryOverlaps box (MultiClaim _ list) = 
    S.fromList . filter (isRight . overlaps box . claimBounds) $ list
queryOverlaps box (SingleClaim _ claim) = 
    case overlaps (claimBounds claim) box of
        Left _ -> S.empty
        Right _ -> S.singleton claim
queryOverlaps box (Branch _ _ al br) = 
    S.union (queryOverlaps box al) (queryOverlaps box br)

claimTreeOverlap :: ClaimTree -> Int
claimTreeOverlap (EmptyTree _) = 0
claimTreeOverlap (MultiClaim (BBox t b l r) _) = (b - t) * (r - l)
claimTreeOverlap (SingleClaim _ _) = 0
claimTreeOverlap (Branch _ _ al br) = claimTreeOverlap al + claimTreeOverlap br

findNonOverlapped :: ClaimTree -> [Int]
findNonOverlapped (EmptyTree _) = []
findNonOverlapped (MultiClaim _ _) = []
findNonOverlapped (SingleClaim bbox (Claim claimid cbox))
    | fromRight (overlaps cbox bbox) == cbox = [claimid]
    | otherwise = []
findNonOverlapped (Branch _ _ al br) = findNonOverlapped al ++ findNonOverlapped br


-- testing
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _) = error "Left value passed to fromRight."

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

bboxInvariants :: BBox -> Bool
bboxInvariants (BBox t b l r) = t < b && l < r

claimInvariants :: Claim -> Bool
claimInvariants (Claim _ bbox) = bboxInvariants bbox

claimTreeInvariants :: ClaimTree -> Bool
claimTreeInvariants (EmptyTree bbox) = bboxInvariants bbox
claimTreeInvariants (SingleClaim bbox claim@(Claim _ claimbox)) = 
    bboxInvariants bbox && claimInvariants claim && isRight (overlaps claimbox bbox)
claimTreeInvariants _ = error "function: claimTreeInvariants is incomplete."
