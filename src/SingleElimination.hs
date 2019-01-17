{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module SingleElimination
  (
    makeTournament
  , pruneTournament
  , showTournament
  , Name
  , Tournament
  , Match(..)
  , Participant(..)
  ) where

import           Data.Tree (Tree (Node), drawTree)

type Name = String

data Participant = Participant Name | Bye | Blank
  deriving (Eq, Show)

data Match = Match Participant Participant
 deriving (Eq, Show)

type Tournament = Tree Match

leafMatchCount :: [Name] -> Int
leafMatchCount participants =
  2 ^ (height - 1)
  where height = ceiling $ logBase 2 count
        count = fromIntegral $ length participants

makeBrackets :: [Name] -> [Match]
makeBrackets participants =
  brackets' startSlots $ Participant <$> participants
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

makeTournament :: [Name] -> Tournament
makeTournament names =
  makeTournament' $ leafNode <$> makeBrackets names

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
        _           -> Blank

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

showTournament :: Tournament -> IO ()
showTournament tournament = putStrLn $ drawTree $ showMatch <$> tournament

showMatch :: Match -> String
showMatch (Match p1 p2) =
  showSide p1 ++ " vs " ++ showSide p2
  where showSide s =
          case s of
            Participant p -> p
            Blank         -> "?"
            _             -> " "
