module Argo.Encoder where

import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.Functor.Identity as Identity

type Encoder = Trans.ReaderT Config (Trans.WriterT Builder.Builder Identity.Identity)

data Config = Config
    { indent :: Indent
    , level :: Int
    } deriving (Eq, Show)

data Indent
    = Spaces Int
    | Tab
    deriving (Eq, Show)

hasIndent :: Config -> Bool
hasIndent x = case indent x of
    Spaces y -> y > 0
    Tab -> True

increaseLevel :: Config -> Config
increaseLevel x = x { level = level x + 1 }
