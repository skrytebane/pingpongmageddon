import           Data.List  (intercalate)
import           Data.Maybe (catMaybes)
import           Data.Tree

type Name = Int

data Participant = Participant Name | Bye | Blank
  deriving (Eq, Show)

data Match = Match Participant Participant
 deriving (Eq, Show)

type Tournament = Tree Match

data Labeled a = Labeled Int a
  deriving (Eq, Show)

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

pruneTournament :: Tournament -> Tournament
pruneTournament (Node r ms) =
  Node r (fmap pruneTournament $ filter isBye ms)
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

vizNode :: Tree (Labeled Match) -> String
vizNode node =
  "graph {\n" ++ (intercalate "\n" $ fmap showRelation $ relations) ++ "\n}"

  where
    relations = catMaybes $ flatten $ vizNode' node

    showNode n = "m" ++ show n

    showRelation (n, ns) =
      "  " ++ showNode n ++ " -- {" ++ (intercalate " " $ (fmap showNode ns)) ++ "}"

    label (Node (Labeled i' _) _) = i'

    childLabels = fmap label

    vizNode' n@(Node (Labeled i m) ms) =
      case ms of
        [] -> Node Nothing []
        ms -> Node (Just (label n, childLabels ms)) $ fmap vizNode' ms

showTournament :: Tournament -> IO ()
showTournament tournament =
  putStrLn $ drawTree $ fmap showMatch tournament
  where showMatch (Match p1 p2) = "[" ++ showParticipant p1 ++ " vs " ++ showParticipant p2 ++ "]"
        showParticipant p =
          case p of
            Blank         -> "?"
            Bye           -> "!"
            Participant n -> show n

main = do
  let tournament = makeTournament $ makeBrackets [1..13]
  -- showTournament $ pruneTournament tournament
  putStrLn $ drawTree $ fmap show $ labelTree $ pruneTournament tournament
