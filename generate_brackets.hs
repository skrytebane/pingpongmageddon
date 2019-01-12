import           Data.Tree

type Name = Int

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
  brackets' startSlots (map Participant participants)
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
  makeTournament' $ fmap leafNode bracket

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

leafNode :: a -> Tree a
leafNode = flip Node []

showTournament :: Tournament -> IO ()
showTournament tournament =
  putStrLn $ drawTree $ fmap showMatch tournament
  where showMatch (Match p1 p2) = "[" ++ showParticipant p1 ++ " / " ++ showParticipant p2 ++ "]"
        showParticipant p =
          case p of
            Blank         -> "?"
            Bye           -> "!"
            Participant n -> show n

main = showTournament $ makeTournament $ makeBrackets [1..5]
