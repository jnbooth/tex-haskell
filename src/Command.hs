module Command (Command(..), unfold) where

import ClassyPrelude

import IRC (IRC)

data Command = Command
    { cmds  :: ![Text]
    , usage :: !Text
    , run   :: !(Text -> IRC (Maybe Text))
    }

unfold :: Command -> [(Text, (Text -> IRC (Maybe Text)))]
unfold Command{..} = (, run) <$> cmds
