module Commands (commands) where

import ClassyPrelude

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import qualified Command
import Command (Command)

import qualified Command.Zyn

commands :: HashMap Text Command
commands = HashMap.fromList $ Command.unfold =<<
    [ Command.Zyn.command

    ]
