module Lib (main) where

import ClassyPrelude

import qualified Control.Concurrent.Forkable as Concurrent
import qualified Configuration.Dotenv as Dotenv
import qualified System.IO as IO
import qualified Data.Text as Text

import qualified Command
import qualified Commands
import qualified Context
import qualified Env
import Env (ENV)
import qualified Failure
import qualified IRC
import IRC (IRC)
import qualified Util

main :: IO ()
main = do
    void $ Dotenv.loadFile Dotenv.defaultConfig
    bracket Env.new (IO.hClose . Env.handle) $ runReaderT run

run :: ENV ()
run = do
    void . Concurrent.forkIO $ forever flush
    nick <- asks Env.nick
    Env.silent "NICK" nick
    Env.silent "USER" $ nick ++ " * * " ++ nick
    forever listen

flush :: ENV ()
flush = do
    env <- ask
    (msg, vis) <- readChan $ Env.out env
    liftIO . IO.hPutStrLn (Env.handle env) $ unpack msg
    when vis . putStrLn $ "\x1b[32m> " ++ msg ++ "\x1b[0m"

listen :: ENV ()
listen = do
    env <- ask
    let botNick = Env.nick env
    response <- (pack <$>) . liftIO . IO.hGetLine $ Env.handle env
    if | "PING :" `isPrefixOf` response ->
          Env.silent "PONG" $ drop 5 response
       | loggedOn botNick `isPrefixOf` response -> do
          password <- Env.getEnv "IRC_PASSWORD"
          Env.silent "PRIVMSG" $ "NickServ IDENTIFY " ++ password
          autojoin <- Env.getEnv "AUTOJOIN"
          for_ (Text.split (== ',') autojoin) $ Env.silent "JOIN" . ("#" ++)
       | otherwise -> do
          let (nick, a)   = Util.breakOn ' ' $ drop 1 response
              (cmd, b)    = Util.breakOn ' ' a
              (chan, msg) = Util.breakOn ' ' b
              cmds        = getCommands $ drop 1 msg
          if | cmd == "PRIVMSG" && not (null cmds) -> do
                  putStrLn $ "\x1b[37m> " ++ response ++ "\x1b[0m"
                  ctx <- liftIO $ Context.new chan nick
                  runReaderT (traverse_ (Concurrent.forkIO . eval) cmds) ctx
             | otherwise -> putStrLn response
  where
    loggedOn nick = ":" ++ nick ++ " MODE " ++ nick

getCommands :: Text -> [Text]
getCommands "" = []
getCommands s
  | '\SOH' `elem` s = [] -- Ignore CTCP
  | firstChar == '.' || firstChar == '!' = [drop 1 s]
  | otherwise = filter (not . null) . 
                catMaybes .
                (rightBracket <$>) .
                drop 1 $
                leftBracket s
  where
    firstChar      = Text.head s
    leftBracket    = Text.splitOn "["
    rightBracket x = flip take x <$> Text.findIndex (== ']') x

eval :: Text -> IRC ()
eval (Util.breakOn ' ' -> (cmd, query)) = case lookup cmd Commands.commands of
    Nothing      -> return ()
    Just command -> do
        res <- Command.run command query
        case fromMaybe (Left Failure.IncorrectUsage) res of
            Right msg -> IRC.reply msg
            Left (Failure.Ambiguous size sample) -> IRC.reply $
                "Did you mean: " ++ intercalate ", " sample ++ 
                " (" ++ tshow size ++ " total)"
            Left Failure.IncorrectUsage -> IRC.reply $
                "Usage: " ++ cmd ++ " " ++ Command.usage command
            Left Failure.NoResults -> IRC.reply
                "I'm sorry, I couldn't find anything."
            Left Failure.Unauthorized -> do
                nick <- asks Context.nick
                putStrLn $ 
                    "Warning! " ++ nick ++ 
                    " attempted to use an unauthorized command: " ++ cmd ++ "!"
            
