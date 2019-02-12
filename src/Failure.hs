module Failure (Failure(..), nonEmpty) where

import ClassyPrelude

data Failure
    = Ambiguous Int [Text]
    | IncorrectUsage
    | NoResults
    | Unauthorized
  deriving (Eq, Ord, Show, Read)

nonEmpty :: ∀ o. MonoFoldable o => o -> Either Failure o
nonEmpty x
  | null x    = Left NoResults
  | otherwise = Right x
