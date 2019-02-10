module Context (Context(..), new) where

import ClassyPrelude

import qualified Data.Time.Clock.System as Time
import Data.Time.Clock.System (SystemTime)

import qualified Util

data Context = Context
    { channel :: !Text
    , host    :: !Text
    , nick    :: !Text
    , user    :: !Text
    , time    :: !SystemTime
    }

new :: Text -> Text -> IO Context
new channel userstuff = do
    time <- Time.getSystemTime
    let (nick, host) = Util.breakOn '!' userstuff
    let user         = toLower nick
    return Context{..}
