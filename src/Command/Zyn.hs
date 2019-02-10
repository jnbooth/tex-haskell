module Command.Zyn (command) where

import ClassyPrelude

import Command (Command(..), abbrev)
import qualified Parse

command :: Command
command = Command
    { cmds  = abbrev "zyn"
    , usage = ""
    , run   = Parse.noArgs $ Right "Marp."
    }
