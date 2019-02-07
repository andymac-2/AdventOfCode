# Advent of code Day 12 2018

This time around, we need to have green thumbs! Our goal is to maintain some plants. Our input file looks something like this:

```
initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #
```

Each `#` is a plant, and each `.` is an empty pot. Each line that looks like `####. => #` is a rule to see what plants in the next genenation will look like. Generation zero is given on the line beginning with `initial state:`. Each generation we use the rules to transform our plants: each plant is dependent on the two plants to the left of it, the two to the right, and the plant directly above. Using the file and rules above, we would get:

```
                 1         2         3     
       0         0         0         0     
 0: ...#..#.#..##......###...###...........
 1: ...#...#....#.....#..#..#..#...........
 2: ...##..##...##....#..#..#..##..........
 3: ..#.#...#..#.#....#..#..#...#..........
 4: ...#.#..#...#.#...#..#..##..##.........
 5: ....#...##...#.#..#..#...#...#.........
 6: ....##.#.#....#...#..##..##..##........
 7: ...#..###.#...##..#...#...#...#........
 8: ...#....##.#.#.#..##..##..##..##.......
 9: ...##..#..#####....#...#...#...#.......
10: ..#.#..#...#.##....##..##..##..##......
11: ...#...##...#.#...#.#...#...#...#......
12: ...##.#.#....#.#...#.#..##..##..##.....
13: ..#..###.#....#.#...#....#...#...#.....
14: ..#....##.#....#.#..##...##..##..##....
15: ..##..#..#.#....#....#..#.#...#...#....
16: .#.#..#...#.#...##...#...#.#..##..##...
17: ..#...##...#.#.#.#...##...#....#...#...
18: ..##.#.#....#####.#.#.#...##...##..##..
19: .#..###.#..#.#.#######.#.#.#..#.#...#..
20: .#....##....#####...#######....#.#..##.
```

We are also given a description of a function which calculates a generation's *value*. The value of any given generation is the sum of all of the plant's positions. If we have a plant at position 0, 3, and 4, our value would be 7. Plants may also be in negative positions. We are asked to caluculate the value after 20 generations. For a complete description of the puzzle, visit [the advent of code website](https://adventofcode.com/2018/day/12).

## Basic datatypes

We create a few datatypes to help us manipulate the plants:

```haskell
-- plant class
data Plant = Plant | NoPlant deriving (Eq)

instance Show Plant where
    show Plant = "#"
    show NoPlant = "."

-- Rules class
type Rule = [Plant]
```

 We create a plant class, which is either `Plant` or `NoPlant`. We could have just as equally used a `newtype` wrapper around `Bool`, but this is just as easy. We instance `Show` for debugging purposes. We also create a `Rule` type synonym, which represents one of the plant rules. We are going to discard any rule that does not produce a plant later on, so we don't need to store if the rule produces a plant or not: the fact that it exist is sufficient.
 
 We also introduce a `Gen` type which will represent a single generation of plants:
 
```haskell
-- Generation class
-- Gen offset plantsList
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
```

The `Gen` type consists of a list of `Plant`, and an `Int`. The `Int` is an offset from zero where our plants start. We don't need to store a large number of empty pots either side of the end, so we use the offset to tell us how far our window of viewing is away from zero.

The `NewGen` function creates a `Gen` from a list of plant and an offst. In the `newGen` function, we strip away empty pots at the start and end, and make sure that there are only 4 empty pots on either side, the offset is adjusted accordingly.

## parsing

Let's move onto parsing the file, for that we'll use `Megaparsec`:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec (Parsec, parse, errorBundlePretty, (<|>), many, eof)
import Text.Megaparsec.Char (space, char, string)

import Data.Void (Void)

import Data.Maybe (catMaybes)
```

`catMaybes` takes a list of `Maybe a`, filters out all of the `Nothing`'s, and returns a list of the unwrapped `a`'s.

```haskell
-- parse input
type Parser = Parsec Void T.Text

parseFile :: T.Text -> (Gen, [Rule])
parseFile str = case parse fileP "" str of
    Left err -> error (errorBundlePretty err)
    Right val -> val
```

Our parser looks pretty basic so far. We want to parse the file, return the initial generation, and a list of rules.

``` haskell
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
```

The rest of the parser is nothing to write home about, we parse a single plant using `plantP`. We parse a `Rule` using `ruleP`. If a rule produces no plant, we return `Nothing`. The `fileP` parser parses the initial state and creates a new `Gen`, and filters out all of the rules that do not produce a plant. The resulting generation and list of rules is returned.

## processing

We have a few functins that operate on the `Gen` type.

```haskell
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
```

`nextGen` given a set of rules will produce the next generation. If the first five elements of a list of plants is found in the list of rules, it will produce a plant. We assume that any rule not found in the list does not produce a plant, since we discarded them during parsing. We iterate through the list of plants to produce a new list, and adjust our offset accordingly.

`generations` simply creates an infinite list of all of the generations. The `value` function calculates the value of a given generation. We're almost done, all we need to do is put it together:

```haskell
main :: IO ()
main = do
    T.interact (T.pack . show . process . parseFile)
    putStrLn ""

-- part 1
process :: (Gen, [Rule]) -> Int
process (gen, rs) = value . (!! 20) . generations rs $ gen
```

All we do is parse `stdin`, create our infinite list of generations so we can take the 20th result, then calculate it's value. We print the result: `3258`. This satisfies part 1 of Day 12.

## Part 2

Part 2 asks for the value of the 50 billionth generation. Obviously there must be some trick to it. If the plants grew indefinitely, then we would need gugabytes of memory just to store it. I figured there must be a pattern to this madness, and pattern there is! If I change my `process` and `main` function around slightly, I can print out the results of the first `100` generations:

```haskell
main :: IO ()
main = do
    T.interact (T.pack . process . parseFile)
    putStrLn ""

-- part 2
process :: (Gen, [Rule]) -> String
process (gen, rs) = unlines . fmap show . take 100 . generations rs $ gen
```

Output:

```
Gen -4 ....#.####...##..#....#####.##.......##.#..###.#####.###.##.###.###.#...#...##.#.##.#...#..#.##..##.#.##....
Gen -5 ....####..##..#..###........#..#......##..#..#.##....###.#.####.###...#.###..##..#.#..#.######....##..#..#....
Gen -2 ....#..#..###..###......######.....#..######..#....#...##...#####.######..#..##..#####....##...#..######....
Gen -3 ....######..##...###..........##...###......#.###..###..#.#.......##....#.###.#.......##...#.#.###......##....

...

Gen 50 ....###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###....###...###....
Gen 51 ....###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###....###...###....
Gen 52 ....###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###....###...###....
Gen 53 ....###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###...###....###...###....
```

At around the generation 95 mark, we start to see a repeating pattern. Each successive generation of plants has the same plant placement, but it keeps shifting one plant over. At generation 100, our value is `9222` and at generation 101 our value is `9294`. It will continue to increase linearly. To calculate the value at 50 billion, we just enter into `ghci`:

```haskell
ghci> (50000000000 - 100) * (9294 - 9222) + 9222
3600000002022
```
And that's Day 12. See you again when I've completed more days!
