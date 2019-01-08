module Commands (eval) where

import ClassyPrelude hiding (handle)

import qualified Data.Maybe as Maybe
import qualified Control.Monad.Trans.Reader as Reader

import qualified Util
import qualified IRC
import IRC (Bot(..), Context(..), IRC)
import Persist

import qualified Dictionary

import CommandsFramework

abbreviate :: [Text]
abbreviate =
    [ "choose"
    , "define"
    , "google"
    , "remindme"
    , "seen"
    , "tell"
    , "wikipedia"
    , "zyn"
    ]

canon :: Text -> Text
canon "lc" = "lastcreated"
canon "sm" = "showmore"
canon x    = fromMaybe x $ find (isPrefixOf x) abbreviate

eval :: [Text] -> IRC ()
eval (x:xs) = run x $ canon x : xs
eval _      = return ()

usage :: Text -> Maybe Text
usage "auth"        = Just " level user"
usage "choose"      = Just " choices, separated, by commas"
usage "define"      = Just " word"
usage "disable"     = Just " command"
usage "enable"      = Just " command"
usage "forget"      = Just " user"
usage "gis"         = Just " query"
usage "google"      = Just " query"
usage "help"        = Just " command"
usage "lastcreated" = Just ""
usage "quit"        = Just ""
usage "reload"      = Just ""
usage "remindme"    = Just " [<days>d][<hours>h][<minutes>m] message (e.g. 4h30m)"
usage "seen"        = Just " [-f|-t] user [#channel]"
usage "showmore"    = Just " number"
usage "tell"        = Just " user message"
usage "wikipedia"   = Just " article"
usage "zyn"         = Just ""
usage _             = Nothing

run :: Text -> [Text] -> IRC ()

run _ ["auth", parse -> Right lvl, nick] = auth (lvl + 1) $ do
    Context{..} <- Reader.ask
    let Bot{..}  = ctxBot
    changeAuth ctxUser (User nick lvl Nothing) botUsers

-- choose TODO

run _ ("define":xs) = Dictionary.search (unwords xs) >>= IRC.tryReply

run _ ["hello"] = IRC.reply "TODO"

run _ ["hug"] = do
    userNick <- Reader.asks $ userNick . ctxUser
    IRC.ctcp "ACTION" $ "hugs " ++ userNick ++ "."

run _ ["quit"]  = auth 4 $ do
    quit <- Util.lookupEnv "QUIT"
    IRC.send "QUIT" $ Maybe.fromMaybe "" quit

run cmd ((usage -> Just s):_) = IRC.reply $ "Usage: " ++ cmd ++ s

run _ _ = return ()
