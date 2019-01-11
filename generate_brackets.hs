type Name = Int

data Bracket = Participant Name | WinnerOf | WalkOver
  deriving (Eq, Show)

leafMatchCount participants =
  2 ^ (height - 1)
  where height = ceiling $ logBase 2 count
        count = fromIntegral $ length participants

brackets participants =
  brackets' startSlots (map Participant participants)
  where
    startSlots = leafMatchCount participants
    brackets' slots bracket =
      case bracket of
        [] -> []
        x : [] -> [(x, WalkOver)]
        x : y : cs ->
          let
            remainingPlayers = length cs
            remainingSlots = slots - 1
          in
            if remainingSlots > remainingPlayers then
              (x, WalkOver) : brackets' remainingSlots (y : cs)
            else
              (x, y) : brackets' remainingSlots cs

nextRound bracket =
  case bracket of
    [] -> []
    m1 : m2 : ms ->
      (winner m1, winner m2) : nextRound ms
  where winner p =
          case p of
            (Participant _, Participant _) -> WinnerOf
            (p1, WalkOver)                 -> p1
            (WalkOver, p1)                 -> p1


tournamentParticipants :: [Name]
tournamentParticipants = [1..13]

foo parts =
  let
    slots = leafMatchCount parts
    leafbracket = brackets parts
  in
    do
      putStrLn $ "To fill: " ++ show slots
      putStrLn $ "Matches: " ++ (show $ length leafbracket)
      print leafbracket
      print $ nextRound leafbracket

main =
  foo tournamentParticipants
