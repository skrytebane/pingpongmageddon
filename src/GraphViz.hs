module GraphViz (makeGraphvizDot) where

import           Data.List         (intercalate)
import           Data.Maybe        (catMaybes)
import           Data.Tree         (Tree (Node), flatten)

import           SingleElimination

data Labeled a = Labeled Int a
  deriving (Eq, Show)

labelTree :: Tournament -> Tree (Labeled Match)
labelTree t =
  decorate t $ reverse [0..length t]

  where
    decorate n l =
      case (n, l) of
        (Node m ms, label' : labels) ->
          Node (Labeled label' m) $ decorateForest ms labels
        _ ->
          undefined

    decorateForest nodes l =
      case nodes of
        x : xs ->
          let (l', l'') = splitAt (length x) l
          in decorate x l' : decorateForest xs l''
        [] -> []

showMatch :: Match -> String
showMatch (Match p1 p2) =
  showSide p1 ++ "|" ++ showSide p2
  where showSide s =
          case s of
            Participant p -> p
            _             -> ""

makeGraphvizDot :: String -> Int -> Tournament -> String
makeGraphvizDot name seed tournament =
  "graph T {\n" ++
  "  // " ++ name ++ "\n" ++
  "  // Seed: " ++ show seed ++ "\n\n" ++
  (intercalate "\n" $ flatten $ nodeConfig <$> node) ++
  "\n\n" ++
  (intercalate "\n" $ showRelation <$> relations)
  ++ "\n}"

  where
    node = labelTree tournament

    nodeConfig (Labeled l m) = "  " ++
      showNode l ++
      " [" ++
      "label=\"#" ++ show l ++ "|{" ++ showMatch m ++ "}\" " ++
      "shape=\"record\" penwidth=2 fontname=\"Helvetica\"];"

    relations = catMaybes $ flatten $ vizNode' node

    showNode n = "m" ++ show n

    showRelation (n, ns) =
      "  " ++ showNode n ++ " -- {" ++ (intercalate " " $ showNode <$> ns) ++ "}"

    nodeLabel (Node (Labeled l _) _) = l

    vizNode' n@(Node _ ms) =
      case ms of
        []  -> Node Nothing []
        ms' -> Node (Just (nodeLabel n, nodeLabel <$> ms')) $ vizNode' <$> ms'
