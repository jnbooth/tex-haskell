module Command.Choose where

import ClassyPrelude

import Data.List ((!!))
import qualified System.Random as Random
import qualified Data.Text as Text

import Command (Command(..), Outcome, abbrev)
import IRC (IRC)
import qualified Parse

command :: Command
command = Command
    { cmds  = abbrev "choose"
    , usage = "<choices, separated, by, commas>"
    , run   = Parse.run choose
    }

choose :: Text -> IRC Outcome
choose s = case filter (not . null) $ Text.split (== ',') s of
  []    -> return $ Right "I choose nothing."
  [opt] -> return $ Right opt
  opts  -> Right . (opts !!) . fst . Random.randomR (0, length opts) <$> 
           liftIO Random.newStdGen
