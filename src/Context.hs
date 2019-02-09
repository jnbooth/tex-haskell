module Context (Context(..), new) where

import ClassyPrelude

import qualified Data.Text as Text
import qualified Data.Time.Clock.System as Time
import Data.Time.Clock.System (SystemTime)

import Env (Env)

data Context = Context
    { env     :: !Env
    , channel :: !Text
    , host    :: !Text
    , nick    :: !Text
    , user    :: !Text
    , time    :: !SystemTime
    }

new :: Env -> Text -> Text -> IO Context
new env channel userstuff = do
    time <- Time.getSystemTime
    let host = "TODO"
    let nick = userstuff
    let user = Text.toLower nick
    return Context{..}
