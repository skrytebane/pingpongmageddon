{-# LANGUAGE OverloadedStrings #-}

module JsonExport where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8)
import           Data.Tree            (Tree (Node))

import           SingleElimination

makeJson :: String -> Int -> Tournament -> String
makeJson name seed tree =
  T.unpack $ decodeUtf8 $ B.toStrict $ encode $ showTournament' tree

  where
    showTournament' tree' =
      object [ "name" .= name
             , "seed" .= seed
             , "brackets" .= showNode tree'
             ]

    showNode (Node (Match p1 p2) children) =
      object [ "a" .= showParticipant p1
             , "b" .= showParticipant p2
             , "children" .= fmap showNode children
             ]

    showParticipant a =
      case a of
        Blank         -> Nothing
        Bye           -> Nothing
        Participant n -> Just n
