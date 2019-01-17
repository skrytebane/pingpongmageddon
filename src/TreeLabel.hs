module TreeLabel (labelTree, Labeled(..)) where

import           Data.Tree (Tree (Node))

-- | A label wrapper
data Labeled l a = Labeled l a
  deriving (Eq, Show)

-- | Return input tree with nodes enclosed in numbered labels.
labelTree :: Tree a -> Tree (Labeled Int a)
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
