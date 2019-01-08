module Main where

import ClassyPrelude
import qualified App
import qualified Persist

import qualified Configuration.Dotenv as Dotenv
import qualified Configuration.Dotenv.Types as Dotenv
import qualified Data.Text.Encoding as Encoding
import qualified System.Environment as Env
import qualified Data.Either as Either
import qualified Database.Persist.Postgresql as Sql
import qualified Control.Monad.Logger as Logger
import qualified Data.Attoparsec.Text as Parse

import Data.Attoparsec.Text (Parser)

data DbInfo = DbInfo
    { dbUser     :: Text
    , dbPassword :: Text
    , dbHost     :: Text
    , dbName     :: Text
    }
dbFmt :: DbInfo -> Text
dbFmt DbInfo{..} = concat
    [ "host="
    , dbHost
    , " dbname="
    , dbName
    , " user="
    , dbUser
    , " password="
    , dbPassword
    , " port=5432"
    ]

connParse :: Parser DbInfo
connParse = do
    void $ Parse.string "postgres://"
    dbUser <- Parse.takeWhile (/= ':')
    void $ Parse.char ':'
    dbPassword <- Parse.takeWhile (/= '@')
    void $ Parse.char '@'
    dbHost <- Parse.takeWhile (/= '/')
    void $ Parse.char '/'
    dbName <- Parse.takeWhile (/= ' ')
    return DbInfo{..}

main :: IO ()
main = do
    void $ Dotenv.loadFile Dotenv.defaultConfig
    dbUrl <- Env.getEnv "DATABASE_URL"
    Logger.runStderrLoggingT . Sql.withPostgresqlPool (unsafeConn dbUrl) 10 $ 
      \pool -> liftIO $ do
          flip Sql.runSqlPersistMPool pool $ Sql.runMigration Persist.migrateAll
          App.main pool
  where
    unsafeConn = 
        Encoding.encodeUtf8 . dbFmt . 
        Either.fromRight (error "Invalid DATABASE_URL") . 
        Parse.parseOnly connParse . pack
