module Main where

import           Data.List          (intercalate)
import           Data.Map           (Map (..), (!))
import qualified Data.Map           as M
import           Data.Maybe         (catMaybes)
import           Data.Tree          (Tree (Node), drawTree, flatten)
import           System.Environment (getArgs)
import           System.Random      (RandomGen (..), genRange, getStdRandom,
                                     mkStdGen, random, randomR)

type Name = String

data Participant = Participant Name | Bye | Blank
  deriving (Eq, Show)

data Match = Match Participant Participant
 deriving (Eq, Show)

type Tournament = Tree Match

data Labeled a = Labeled Int a
  deriving (Eq, Show)

label :: Labeled a -> Int
label (Labeled a _) = a

leafMatchCount :: [Name] -> Int
leafMatchCount participants =
  2 ^ (height - 1)
  where height = ceiling $ logBase 2 count
        count = fromIntegral $ length participants

makeBrackets :: [Name] -> [Match]
makeBrackets participants =
  brackets' startSlots (fmap Participant participants)
  where
    startSlots = leafMatchCount participants
    brackets' slots bracket =
      case bracket of
        [] -> []
        x : [] -> [Match x Bye]
        x : y : cs ->
          let
            remainingPlayers = length cs
            remainingSlots = slots - 1
          in
            if remainingSlots > remainingPlayers then
              Match x Bye : brackets' remainingSlots (y : cs)
            else
              Match x y : brackets' remainingSlots cs

makeTournament :: [Match] -> Tournament
makeTournament bracket =
  makeTournament' $ leafNode <$> bracket

  where
    makeTournament' forest =
      case forest of
        n : [] -> n
        ns     -> makeTournament' $ nextRound ns

    nextRound forest =
      case forest of
        n1@(Node m1 _) : n2@(Node m2 _) : ms ->
          Node (Match (winner m1) (winner m2)) [n1, n2] : nextRound ms
        _ -> []

    winner match =
      case match of
        Match p Bye -> p
        Match Bye p -> p
        m           -> Blank

pruneTournament :: Tournament -> Tournament
pruneTournament (Node r ms) =
  Node r (pruneTournament <$> filter isBye ms)
  where isBye (Node m _) =
          case m of
            Match Bye _ -> False
            Match _ Bye -> False
            _           -> True

leafNode :: a -> Tree a
leafNode = flip Node []

labelTree :: Tournament -> Tree (Labeled Match)
labelTree t =
  decorate t $ reverse [0..length t]

  where
    decorate (Node m ms) (label : labels) =
      Node (Labeled label m) $ decorateForest ms labels

    decorateForest nodes l =
      case nodes of
        x : xs ->
          let (l', l'') = splitAt (length x) l
          in decorate x l' : decorateForest xs l''
        [] -> []

makeGraphvizDot :: String -> Int -> Tree (Labeled Match) -> String
makeGraphvizDot name seed node =
  "graph T {\n" ++
  "  // " ++ name ++ "\n" ++
  "  // Seed: " ++ show seed ++ "\n\n" ++
  (intercalate "\n" $ flatten $ nodeConfig <$> node) ++
  "\n\n" ++
  (intercalate "\n" $ showRelation <$> relations)
  ++ "\n}"

  where
    nodeConfig (Labeled l m) = "  " ++
      showNode l ++
      " [" ++
      "label=\"{" ++ showMatch m ++ "}\" " ++
      "shape=\"record\" penwidth=2 fontname=\"Helvetica\"];"

    relations = catMaybes $ flatten $ vizNode' node

    showNode n = "m" ++ show n

    showRelation (n, ns) =
      "  " ++ showNode n ++ " -- {" ++ (intercalate " " $ showNode <$> ns) ++ "}"

    nodeLabel (Node l _) = label l

    childLabels = fmap nodeLabel

    vizNode' n@(Node (Labeled i m) ms) =
      case ms of
        [] -> Node Nothing []
        ms -> Node (Just (nodeLabel n, childLabels ms)) $ vizNode' <$> ms

showTournament :: Tournament -> IO ()
showTournament tournament = putStrLn $ drawTree $ showMatch <$> tournament

showMatch (Match p1 p2) =
  showSide p1 ++ "|" ++ showSide p2
  where showSide s =
          case s of
            Participant p -> p
            _             -> ""

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
    initial x gen = (M.singleton 0 x, gen)

shufflePlayers :: Int -> [Name] -> [Name]
shufflePlayers seed players =
  let
    gen = mkStdGen seed
    (shuffled, _) = fisherYates gen players
  in
    shuffled

main = do
  seed <- getStdRandom random
  players <- lines <$> getContents
  name : _ <- getArgs
  let tournament = pruneTournament $ makeTournament $ makeBrackets $ shufflePlayers seed players
  putStrLn $ makeGraphvizDot name seed $ labelTree $ tournament
