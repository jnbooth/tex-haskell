module Command.Choose (command) where

import ClassyPrelude

import Data.List ((!!))
import qualified System.Random as Random
import qualified Data.Text as Text

import Command (Command(..), Outcome, abbrev)
import qualified IRC
import IRC (IRC)
import qualified Parse

command :: Command
command = Command
    { cmds  = abbrev "choose"
    , usage = "<choices, separated, by, commas>"
    , run   = Parse.run choose
    }

choose :: Text -> IRC Outcome
choose s = Right <$> case filter (not . null) $ Text.split (== ',') s of
    []    -> IRC.reply "I choose nothing."
    [opt] -> IRC.reply opt
    opts  -> IRC.reply . (opts !!) . fst . Random.randomR (0, length opts) =<<
             liftIO Random.newStdGen
