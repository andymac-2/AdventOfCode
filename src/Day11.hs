import Data.List (maximumBy)
import Data.Ord (comparing)

import Data.Function.Memoize (memoFix3)
import Data.Function (fix)

serialNumber :: Int
serialNumber = 3214

gridWidth :: Int
gridWidth = 301

gridHeight :: Int
gridHeight = 301

main :: IO ()
main = do
    let coords = do
        x <- [1..gridWidth]
        y <- [1..gridHeight]
        n <- [1..min (gridWidth - x) (gridHeight - y)]
        return (x, y, n)

    putStrLn . show . maximumBy (comparing (\(x, y, n) -> powerLevelNByN x y n)) $ coords
    putStrLn ""

powerLevel :: Int -> Int -> Int
powerLevel x y = let
    rackID = x + 10
    powerLevelStart = rackID * y + serialNumber
    final = powerLevelStart * rackID
    hundreds = (final `quot` 100) `rem` 10
    in hundreds - 5

powerLevelNByN :: Int -> Int -> Int -> Int
powerLevelNByN = memoFix3 (\this x y n -> let
    centre = this (x + 1) (y + 1) (n - 2)
    -- corners: top left, bottom right etc.
    tl = powerLevel x y
    tr = powerLevel (x + n - 1) y
    bl = powerLevel x (y + n - 1)
    br = powerLevel (x + n - 1) (y + n - 1)
    -- blocks: tlb: top left block etc.
    tlb = this x y (n - 1)
    trb = this (x + 1) y (n - 1)
    blb = this x (y + 1) (n - 1)
    brb = this (x + 1) (y + 1) (n - 1)
    in case n of
        1 -> powerLevel x y
        2 -> tl + tr + bl + br
        _ -> (tl + tr + bl + br + tlb + trb + blb + brb - (2 * centre)) `quot` 2
    )
