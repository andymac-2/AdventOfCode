# Advent of Code 2018 Day 1:

Considering that it's the first day, this problem is pretty simple.

## Part 1

Given a list of signed decimal integers in a text file, find their sum. Our input file looks somthing like so:

```haskell
+5
-11
-1
-16
+12
+9
-15
+20
+4
+17
-4
-14
+6
-17
```

We don't need to do anything particularly special, however, the built in instance `read` for the `Int` type does not support a `+` before an integer. We have to create a custom function for parsing an integer with a sign:

```haskell
parseInt :: String -> Int
parseInt (x: xs) = let
    absolute = read xs
    in case x of
        '+' -> absolute
        '-' -> -absolute
```

This function is pretty hacky, but it gets the job done. If we know that there is a sign, and that it's always the first character, then we can use `read` on the tail of the list, and apply the sign accordingly. Note that there are a lot of things that can go wrong with this function if we don't have complete control over our user input. `read` could fail, and both the case expression and `parseInt` itself do not have exhaustive pattern matches.

The reaminder of the function can be implemented in `main`:

```haskell
main :: IO ()
main = interact (show . sum . fmap parseInt . lines)
```

We split the input into lines, parse each line, sum the result, and convert it to a `String`. The `interact` function is useful if you want to create a unix style program which takes an input and produces an output. `interact` takes one argument: a function that takes a `String` and returns a `String`. The `String` argument is taken from `stdin` and the `String` return value is output to `stdout`.

## Part 2

For part 2: we want to sum the first `n` numbers of the list, and then find the first sum that appears twice. If we reach the end of the list of numbers given, and we haven't seen a duplicate yet, we repeat the list from the start. For that, let's import `Data.Set` from the `containers` package:

```haskell
import Data.Set as S

firstDup :: [Int] -> Int
firstDup xs = go S.empty xs where
    go elems (x: xs) = case x `member` elems of
        True -> x
        False -> go (S.insert x elems) xs
```

This function tries to find the first duplicate element of an infinite list. All we do is create a `Set` of numbers we've already seen before, which starts off empty. If the next number is in the set, then that's the first duplicate, otherwise, add that number to the set, move to the next value, and run the function recursively.

We also need to change our `main`:

```haskell
main :: IO ()
main = interact (show . firstDup . scanl (+) 0 . cycle . fmap parseInt . lines)
```

We still separate the input into lines and parse each line, however this time we create an infinite list using `cycle` which repeats our list infinitely. `scanl (+) 0` takes a running sum of our input, which we toss into our `firstDup` function. Due to Haskell's laziness, `scanl (+) 0` will not have to store every sum encountered so far, and neither will `cycle` hang tying to store an infinite data structure in memory.

A simple start to the advent calendar. See you for day 2!
