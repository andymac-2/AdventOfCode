import Data.List

main :: IO ()
main = interact (show . checksumAll . fmap checksum . lines)

checksum :: Ord a => [a] -> (Bool, Bool)
checksum l = (2 `elem` groupLengths, 3 `elem` groupLengths) where
    groupLengths = fmap length . group . sort $ l

checksumAll :: [(Bool, Bool)] -> Int
checksumAll l = pairs * triplets where
    pairs = length . filter fst $ l
    triplets = length . filter snd $ l
