module Lib (main) where

import ClassyPrelude hiding (handle)

import qualified Data.Char as Char
import qualified Data.CaseInsensitive as CI
import qualified Control.Concurrent.Forkable as Concurrent
import qualified Configuration.Dotenv as Dotenv
import qualified System.IO as IO
import qualified Data.Text as Text

import qualified Command
import qualified Commands
import qualified Context
import qualified Env
import Env (ENV, Env(..))
import qualified Failure
import qualified IRC
import IRC (IRC)
import qualified Util

main :: Bool -> IO ()
main online = do
    void $ Dotenv.loadFile Dotenv.defaultConfig
    bracket env (IO.hClose . Env.handle) $ runReaderT run
  where
    env = if online then Env.new else Env.offline

run :: ENV ()
run = do
    env <- ask
    void . liftIO . Concurrent.forkIO . forever $ flush env
    let nick = CI.original $ Env.nick env
    Env.silent "NICK" nick
    Env.silent "USER" $ nick ++ " * * " ++ nick
    putStrLn "Awaiting input."
    forever listen

flush :: Env -> IO ()
flush Env{handle, out} = do
    (msg, vis) <- readChan out
    writable   <- IO.hIsWritable handle
    when writable . IO.hPutStrLn handle $ unpack msg
    when vis . putStrLn $ "\x1b[32m> " ++ msg ++ "\x1b[0m"

listen :: ENV ()
listen = do
    Env{handle, nick} <- ask
    response <- (pack <$>) . liftIO . IO.hGetLine $ handle
    if | "PING :" `isPrefixOf` response ->
          Env.silent "PONG" $ drop 5 response
       | loggedOn (CI.original nick) `isPrefixOf` response -> do
          password <- Env.getEnv "IRC_PASSWORD"
          Env.silent "PRIVMSG" $ "NickServ IDENTIFY " ++ password
          autojoin <- Env.getEnv "AUTOJOIN"
          for_ (Text.split (== ',') autojoin) $ Env.silent "JOIN" . ("#" ++)
       | otherwise -> do
          let (user, a)   = Util.breakOn ' ' $ drop 1 response
              (cmd, b)    = Util.breakOn ' ' a
              (chan, msg) = Util.breakOn ' ' b
              cmds        = (unCtrl <$>) . getCommands $ drop 1 msg
          if | cmd == "PRIVMSG" && not (null cmds) -> do
                  putStrLn $ "\x1b[37m> " ++ response ++ "\x1b[0m"
                  ctx <- liftIO $ Context.new chan user
                  runReaderT (for_ cmds $ Concurrent.forkIO . eval) ctx
             | otherwise -> putStrLn response
  where
    loggedOn bot = ":" ++ bot ++ " MODE " ++ bot
    unCtrl = filter $ not . Char.isControl

getCommands :: Text -> [Text]
getCommands "" = []
getCommands (uncons -> Just ('.', s)) = [s]
getCommands (uncons -> Just ('!', s)) = [s]
getCommands s
  | '\SOH' `elem` s = [] -- Ignore CTCP
  | otherwise       = filter (not . null) . 
                      catMaybes .
                      (rightBracket <$>) .
                      drop 1 $
                      leftBracket s
  where
    leftBracket    = Text.splitOn "["
    rightBracket x = flip take x <$> Text.findIndex (== ']') x

eval :: Text -> IRC ()
eval (Util.breakOn ' ' -> (cmd, query)) = case lookup cmd Commands.commands of
    Nothing      -> return ()
    Just command -> do
        res <- Command.run command $ Text.strip query
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
                nick <- CI.original <$> asks Context.nick
                putStrLn $ 
                    "Warning! " ++ nick ++ 
                    " attempted to use an unauthorized command: " ++ cmd ++ "!"
            
