module Main where

import           Data.List          (nub, null)
import           Data.Map           (Map, (!))
import qualified Data.Map           as M
import           System.Environment (getArgs)
import           System.Exit        (die)
import           System.Random      (RandomGen (..), getStdRandom, mkStdGen,
                                     random, randomR)

import           GraphViz
import           SingleElimination

-- Fisher-Yates shuffle from https://wiki.haskell.org/Random_shuffle#Purely_functional
fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1..]
    initial x gen' = (M.singleton 0 x, gen')

shufflePlayers :: Int -> [Name] -> [Name]
shufflePlayers seed players =
  let
    gen = mkStdGen seed
    (shuffled, _) = fisherYates gen players
  in
    shuffled

parseArgs :: IO String
parseArgs = do
  args <- getArgs
  case args of
    [] -> die "Usage: generate_brackets <name>\n\nTakes a list of players on stdin.)"
    x : _ -> return x

getPlayers :: IO [String]
getPlayers = do
  players <- filter (not . null) <$> lines <$> getContents
  case players of
    [] -> die "No players specified!"
    p | unique p  -> return p
      | otherwise -> die "Player names must be unique!"

unique :: Eq a => [a] -> Bool
unique l = l == nub l

main :: IO ()
main = do
  seed <- getStdRandom random
  name <- parseArgs
  players <- getPlayers
  let tournament = pruneTournament $ makeTournament $ shufflePlayers seed players
  putStrLn $ makeGraphvizDot name seed $ tournament
