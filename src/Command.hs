module Command (Command(..), abbrev, unfold) where

import ClassyPrelude

import qualified Data.Text as Text

import Failure (Failure)
import IRC (IRC)

type Outcome = Either Failure Text

abbrev :: Text -> [Text]
abbrev = drop 1 . Text.inits

data Command = Command
    { cmds  :: ![Text]
    , usage :: !Text
    , run   :: !(Text -> IRC (Maybe Outcome))
    }

unfold :: Command -> [(Text, Command)]
unfold x = (, x) <$> cmds x
