module Failure (Failure(..)) where

import ClassyPrelude

data Failure
    = Ambiguous Int [Text]
    | IncorrectUsage
    | NoResults
    | Unauthorized
