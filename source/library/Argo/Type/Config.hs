module Argo.Type.Config where

import qualified Argo.Type.Indent as Indent

data Config = Config
    { indent :: Indent.Indent
    , level :: Int
    } deriving (Eq, Show)

initial :: Config
initial = Config
    { indent = Indent.Spaces 0
    , level = 0
    }

hasIndent :: Config -> Bool
hasIndent x = case indent x of
    Indent.Spaces y -> y > 0
    Indent.Tab -> True

increaseLevel :: Config -> Config
increaseLevel x = x { level = level x + 1 }
