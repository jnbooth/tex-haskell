module Context (Context(..), new, target) where

import ClassyPrelude

import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import qualified Data.Time.Clock.System as Time
import Data.Time.Clock.System (SystemTime)

import qualified Util

data Context = Context
    { channel :: !Text
    , nick    :: !(CI Text)
    , host    :: !Text
    , time    :: !SystemTime
    }
  deriving (Eq, Ord, Show)

target :: Context -> Text
target Context{channel, nick} = case headMay channel of
    Just '#' -> channel
    _        -> CI.original nick

new :: Text -> Text -> IO Context
new channel user = Context channel (CI.mk nick) host <$> Time.getSystemTime
  where
    (nick, host) = Util.breakOn '!' user
