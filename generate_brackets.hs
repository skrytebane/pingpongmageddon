import           Data.Tree

type Name = Int
type MatchId = Int

data Participant = Participant Name | Bye | Blank
  deriving (Eq, Show)

data Match = Match Participant Participant
 deriving (Eq, Show)

type Tournament = Tree Match

leafMatchCount participants =
  2 ^ (tournamentHeight participants - 1)

totalMatches participants =
  (2 ^ tournamentHeight participants) - 1

tournamentHeight participants =
  ceiling $ logBase 2 count
  where count = fromIntegral $ length participants

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

showTournament :: Tournament -> IO ()
showTournament tournament =
  putStrLn $ drawTree $ fmap showMatch tournament
  where showMatch (Match p1 p2) = "[" ++ showParticipant p1 ++ " / " ++ showParticipant p2 ++ "]"
        showParticipant p =
          case p of
            Blank         -> "?"
            Bye           -> "!"
            Participant n -> show n

makeTournament :: [Match] -> Tournament
makeTournament bracket =
  allRounds $ forests bracket

  where
    allRounds forest =
      case forest of
        n : [] -> n
        ns     -> allRounds $ nextRound ns
    forests = fmap rootNode
    rootNode m = Node m []
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

tournamentParticipants :: [Name]
tournamentParticipants = [1..13]

foo parts =
  let
    slots = leafMatchCount parts
    leafbracket = makeBrackets parts
  in
    do
      putStrLn $ "To fill: " ++ show slots
      putStrLn $ "Matches: " ++ (show $ length leafbracket)
      print leafbracket

main =
  foo tournamentParticipants
