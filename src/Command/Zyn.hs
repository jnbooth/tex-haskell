module Command.Zyn (command) where

import ClassyPrelude

import Command (Command(..), abbrev)
import qualified IRC
import qualified Parse

command :: Command
command = Command
    { cmds  = abbrev "zyn"
    , usage = ""
    , run   = Parse.noArgs $ Right <$> IRC.reply "Marp."
    }
