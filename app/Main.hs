module Main where

import ClassyPrelude

import qualified System.Environment as SysEnv

import qualified Lib

main :: IO ()
main = do
    args <- SysEnv.getArgs
    Lib.main $ "-o" `notElem` args
