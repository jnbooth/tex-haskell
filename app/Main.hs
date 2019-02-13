module Main where

import ClassyPrelude

import qualified Configuration.Dotenv as Dotenv
import qualified System.Environment as SysEnv

import qualified Lib

main :: IO ()
main = do
    args <- SysEnv.getArgs
    void $ Dotenv.loadFile Dotenv.defaultConfig
    Lib.main $ "-o" `notElem` args
