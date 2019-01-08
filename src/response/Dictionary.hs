module Dictionary (search) where

import qualified Data.MultiMap as MultiMap
import qualified Text.XML.Cursor as XML

import Data.MultiMap (MultiMap)
import Text.XML.Cursor (($//), (&|), (&//))

import ClassyPrelude
import IRC

stringify :: Text -> MultiMap Text Text -> Text
stringify word defs = concat $
    ["\x02", word, "\x02"] ++ do
        (k, vs) <- MultiMap.assocs defs
        [" \x1d(", k, ")\x1d"] ++ do
            (i, v) <- zip [1..] vs
            [" ", tshow i, ". ", v]

--parse :: XML.Cursor -> [(Text, Text)]
--parse doc = do

search :: Text -> IRC (Maybe Text)
search s = do
    page <- IRC.get $ "http://ninjawords.com/" ++ s
    return $ Just "TODO"
