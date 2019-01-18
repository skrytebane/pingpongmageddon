module Main where

import           Data.List           (nub, null, sort)
import           Data.Map            (Map, (!))
import qualified Data.Map            as M
import           Data.Monoid         ((<>))
import           Options.Applicative ((<**>))
import qualified Options.Applicative as P
import           System.Environment  (getArgs)
import           System.Exit         (die)
import           System.Random       (RandomGen (..), getStdRandom, mkStdGen,
                                      random, randomR)

import           GraphVizExport
import           JsonExport
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
shufflePlayers seed' players =
  let
    gen = mkStdGen seed'
    (shuffled, _) = fisherYates gen $ sort players
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

data OutputType = GraphViz | JSON

data CmdArgs = CmdArgs
  {
    outputType :: OutputType
  , seed       :: Int
  , name       :: String
  , noPrune    :: Bool
  }

parseOpts :: Int -> IO CmdArgs
parseOpts defaultSeed = P.execParser opts
  where
    opts = P.info (optParser <**> P.helper)
           ( P.fullDesc
             <> P.progDesc "Generate a single-elimination tournament bracket"
             <> P.header "pingpongmageddon")

    optParser :: P.Parser CmdArgs
    optParser = CmdArgs
      <$> P.flag GraphViz JSON (P.long "json"
                                <> P.short 'j'
                                <> P.help "Output JSON format tournament instead of GraphViz")
      <*> P.option P.auto (P.long "seed"
                           <> P.short 's'
                           <> P.value defaultSeed
                           <> P.metavar "INTEGER"
                           <> P.help "Specify a fixed random seed")
      <*> P.strOption (P.long "name"
                       <> P.short 'n'
                       <> P.value "Pingpongmageddon"
                       <> P.metavar "NAME"
                       <> P.help "Specify name of tournament")
      <*> P.switch (P.long "no-prune"
                   <> P.short 'N'
                   <> P.help "Don't prune tournament")

main :: IO ()
main = do
  defaultSeed <- getStdRandom random
  CmdArgs outputType' seed' name' noPrune' <- parseOpts defaultSeed
  players <- getPlayers
  let pruner = if noPrune' then id else pruneTournament
      tournament = pruner $ makeTournament $ shufflePlayers seed' players
      outputFilter = case outputType' of
        GraphViz -> makeGraphvizDot
        JSON     -> makeJson
  putStrLn $ outputFilter name' seed' tournament
