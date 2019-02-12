module Command.Define where

import ClassyPrelude

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import Data.MultiMap (MultiMap)
import qualified Data.Text as Text

import Command (Command(..), Outcome, abbrev)
import qualified Env
import qualified Failure
import qualified IRC
import IRC (IRC)
import qualified Parse
import qualified Util

command :: Command
command = Command
    { cmds  = abbrev "define"
    , usage = "<query>"
    , run   = Parse.run search
    }

url :: Text
url = "https://www.dictionaryapi.com/api/v3/references/collegiate/json/"

search :: Text -> IRC Outcome
search s = do
    app_key <- Env.getEnv "DICTIONARY_KEY"
    page    <- IRC.json $ url ++ s ++ "?key=" ++ app_key
    let defs = parse page
    return $
      if | MultiMap.null defs -> Left Failure.NoResults
         | otherwise          -> Right $ fmtDefs defs

parse :: Aeson.Value -> MultiMap Text Text
parse (Aeson.Array nodes) = snd . foldl' acc ("other", MultiMap.empty) $
                            concatMap properties nodes
  where
    properties (Aeson.Object node) = HashMap.toList node
    properties _ = []
    acc (_, xs) ("fl",  Aeson.String p)     = (p, xs)
    acc (p, xs) ("def", parseDef -> Just x) = (p, MultiMap.insert p x xs)
    acc x _                                 = x
parse _ = MultiMap.empty

parseDef :: Aeson.Value -> Maybe Text
parseDef (Aeson.Array nodes) = headMay $ do
    Aeson.Object node  <- toList nodes
    Aeson.Array  sseq0 <- toList $ lookup "sseq" node
    Aeson.Array  sseq1 <- toList sseq0
    Aeson.Array  sseq2 <- toList sseq1
    Aeson.Object sseq3 <- toList sseq2
    Aeson.Array  dt0   <- toList $ lookup "dt" sseq3
    Aeson.Array  dt1   <- toList dt0
    Aeson.String def   <- toList $ lastMay dt1
    let cleanDef        = clean def
    guard . not $ null cleanDef
    return cleanDef
parseDef _ = Nothing

clean :: Text -> Text
clean = Text.strip . Util.omitParens . takeWhile (/= '{') . drop 4

fmtDefs :: MultiMap Text Text -> Text
fmtDefs xs
  | fmtd `Util.atMost` 300 = fmtd
  | otherwise              = fmtParts $ second (take 1) <$> tups
  where
    tups = (second reverse <$>) . reverse . Map.toList $ MultiMap.toMap xs
    fmtd = fmtParts tups

    fmtParts        = unwords . (fmtPart <$>)
    fmtPart (k, vs) = fmtLabel k ++ unwords (zipWith fmtDef [1..] vs)
    fmtLabel k      = IRC.italic $ "(" ++ k ++ ") "
    fmtDef i v      = IRC.bold (tshow i ++ ". ") ++ v
