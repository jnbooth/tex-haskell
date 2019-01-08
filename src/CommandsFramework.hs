module CommandsFramework where

import ClassyPrelude hiding (handle)

import qualified Control.Monad.Logger
import qualified Control.Monad.Trans.Resource.Internal
import qualified Focus
import qualified Data.Maybe as Maybe
import qualified Data.Text.Read as Parse
import qualified Control.Monad.Trans.Reader as Reader
import qualified Database.Persist.Postgresql as Sql
import qualified STMContainers.Map as Stm

import IRC (Bot(..), Context(..), IRC)
import Persist

parse :: ∀ a. Integral a => Text -> Either String a
parse = (fst <$>) . Parse.decimal

debug :: ∀ m. MonadIO m => Text -> m ()
debug text = putStrLn $ "\x1b[34m" ++ text ++ "\x1b[0m"

auth :: Int -> IRC () -> IRC ()
auth 0 f = f
auth lvl f = do
    Context{..} <- Reader.ask
    let User{..} = ctxUser
    if | userAuth >= lvl -> f
       | Maybe.maybe False (userNick ==) $ botOwner ctxBot -> f
       | otherwise -> debug $ userNick ++ " tried to use a forbidden command."

changeAuth :: User -> User -> Stm.Map Text User -> IRC ()
changeAuth source target users = do
    muser <- atomically $ Stm.focus (return . strategy) nick users
    case muser of
      Nothing -> return ()
      Just user -> void . runDB $ Sql.repsert (UserKey nick) user
  where
    nick = toLower $ userNick target 
    insertGet x = (Just x, Focus.Replace x)
    strategy (Just user)
      | userAuth user >= userAuth source = (Nothing, Focus.Keep)
      | otherwise = insertGet user { userAuth = userAuth target }
    strategy Nothing = insertGet target

runDB :: ∀ a. ReaderT Sql.SqlBackend
         ( Control.Monad.Logger.NoLoggingT
           (Control.Monad.Trans.Resource.Internal.ResourceT IO)
         ) a -> IRC a
runDB f = do
    botPool <- Reader.asks $ botPool . ctxBot
    liftIO $ Sql.runSqlPersistMPool f botPool
    