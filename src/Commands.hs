module Commands (commands) where

import ClassyPrelude

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import qualified Command
import IRC (IRC)

commands :: HashMap Text (Text -> IRC (Maybe Text))
commands = HashMap.fromList $ Command.unfold =<<
    [

    ]
