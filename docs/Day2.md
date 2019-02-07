# Advent of Code 2018 Day 2:

## Part 1

For this part, we have to calculate a checksum of some strings. We need two numbers `a` and `b`. `a` is the number of strings which have at least one character that appears exactly twice. `b` is the number of strings which have at least one character that appears exactly three times. We multiply these numbers together to get the result. Here is our program.

```haskell
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
```

The `checksum` function takes a list of elements, sorts them, and groups them by equality. We then take the length of all of these groups so we can tell if there is a pair or a triplet. If at least one of the group lengths is 2, then we have a pair, similarly if at least one of the group lengths is 3, then we have a triplet. We then return a `(Bool, Bool)` to say whether or not our string had at least one pair, or at least one triplet.

The `checksumAll` function takes the resulting list of `(Bool, Bool)`, and counts how many pairs and triplets there are. The result is the product of the pairs and triplets. Running our short program gives the ocrrect answer.

## Part 2
