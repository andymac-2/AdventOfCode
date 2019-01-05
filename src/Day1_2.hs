import Data.List

main :: IO ()
main = interact (show . checksumAll . concat . fmap checksum . lines)

uniq :: Ord a => [a] -> [a]
uniq = fmap head . group . sort

checksum :: Ord a => [a] -> [Int]
checksum = tail . uniq . fmap length . group . sort

checksumAll :: [Int] -> Int
checksumAll = product . fmap length . group . sort
