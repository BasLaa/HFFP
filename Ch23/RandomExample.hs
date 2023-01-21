module RandomExample where

import System.Random
import Control.Applicative (liftA3) 
import Control.Monad (replicateM) 
import Control.Monad.Trans.State

-- Six-sided die
data Die =
    One
  | Two
  | Three
  | Four
  | Five
  | Six
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
    1 -> One
    2 -> Two
    3 -> Three
    4 -> Four
    5 -> Five
    6 -> Six
    x -> 
        error $ "intToDie got non 1-6 integer: " ++ show x


rollDie :: State StdGen Die 
rollDie = intToDie <$> state (randomR (1, 6))

nDie :: Int -> State StdGen [Die] 
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int 
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int 
    go sum count gen
      | sum >= 20 = count 
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen


rollsToGetN :: Int -> StdGen -> Int 
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int 
    go sum count gen
      | sum >= n = count 
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die]) 
rollsCountLogged n g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die]) 
    go sum count rolls gen
      | sum >= n = (count, rolls)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
        in go (sum + die) (count + 1) ((intToDie die) : rolls) nextGen
