{-# LANGUAGE RankNTypes #-}
import Control.Monad.State (StateT, evalStateT, get, put)

import Control.Monad.ST (ST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Debug.Trace (traceShowId)

puzzleInput :: Int
puzzleInput = 290431

part2Input :: V.Vector Int
part2Input = V.fromList [5, 1, 5, 8, 9] --[2, 9, 0, 4, 3, 1]

part2VectorLength = V.length part2Input

part2Length :: Int
part2Length = 100000000

windowSize :: Int
windowSize = 10

data RecipeState = RecipeState Int Int Int

main :: IO ()
main = putStrLn recipes'

evalRecipeArray :: (forall s. StateT RecipeState (ST s) (VM.MVector s a)) -> RecipeState -> (V.Vector a)
evalRecipeArray action state = let
    stAction = evalStateT action $ state
    in V.create stAction

recipes' :: String
recipes' = let
    initialState = (RecipeState 0 1 2)
    action = do
        arr <- VM.new (part2Length)
        VM.write arr 0 3
        VM.write arr 1 7
        steps' arr
    result = evalRecipeArray action initialState
    slices = fmap (\x -> (x, V.slice x part2VectorLength result)) $ [1..]

    position = head . filter (\x -> snd x == part2Input) $ slices
    in show position 

steps' :: VM.STVector s Int -> StateT RecipeState (ST s) (VM.MVector s Int)
steps' arr = do
    (RecipeState _ _ len) <- get
    if len > part2Length - 2
        then do
            return arr
        else do
            step arr
            steps' arr

recipes :: String
recipes = let
    initialState = (RecipeState 0 1 2)
    action = do
        arr <- VM.new (puzzleInput + (2 * windowSize))
        VM.write arr 0 3
        VM.write arr 1 7
        steps arr
    in show $ evalRecipeArray action initialState       

steps :: VM.STVector s Int -> StateT RecipeState (ST s) (VM.MVector s Int)
steps arr = do
    (RecipeState _ _ len) <- get
    if len > (puzzleInput + windowSize)
        then do
            return $ (VM.slice puzzleInput (windowSize) arr)
        else do
            step arr
            steps arr

step :: VM.STVector s Int -> StateT RecipeState (ST s) ()
step arr = do
    (RecipeState c1 c2 len) <- get
    rec1 <- VM.read arr c1
    rec2 <- VM.read arr c2
    let (tens, ones) = ((rec1 + rec2) `quotRem` 10)

    VM.write arr len ones
    len' <- case tens of 
        0 -> (len + 1) <$ VM.write arr len ones
        _ -> (len + 2) 
            <$ VM.write arr len tens
            <* VM.write arr (len + 1) ones

    let c1' = (c1 + rec1 + 1) `rem` len'
        c2' = (c2 + rec2 + 1) `rem` len'
    
    put (RecipeState c1' c2' len')
