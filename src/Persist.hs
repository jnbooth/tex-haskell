{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Persist where
import ClassyPrelude
import Database.Persist.TH
import qualified Database.Persist.Postgresql as Sql

import Local(Local)

type Val record =
    ( Sql.PersistEntity record
    , Sql.PersistEntityBackend record ~ Sql.BaseBackend Sql.SqlBackend
    )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    nick Text
    auth Int default=0
    pronouns Text Maybe
    Primary nick
Page
    url    Text
    name   Text
    author Text
    votes  Int
    Primary url
Reminder
    nick    Text
    when    UTCTime
    message Text
Seen
    who         Local
    first       Text
    first_time  UTCTime default=now()
    latest      Text
    latest_time UTCTime default=now()
    total       Int default=1
    Primary who
Silence
    command Local
    Primary command
Tell
    from Text
    to   Text
    when UTCTime default=now()
|]
