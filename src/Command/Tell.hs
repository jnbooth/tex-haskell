module Command.Tell (command) where

import ClassyPrelude

import qualified Data.CaseInsensitive as CI

import Command (Command(..), Outcome, abbrev)
import qualified Env
import qualified IRC
import IRC (IRC)
import qualified Parse
import Parse (TextWord(..))

command :: Command
command = Command
    { cmds  = abbrev "tell"
    , usage = "<user> <message>"
    , run   = Parse.run tell
    }

tell :: TextWord -> Text -> IRC Outcome
tell (TextWord user) message = do
    tells <- lift $ asks Env.tells
    atomically . modifyTVar' tells . insertSet $ CI.mk user
    Right <$> IRC.action "Message sent."
