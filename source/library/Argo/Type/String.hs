module Argo.Type.String where

import qualified Data.Text as Text

newtype String
    = String Text.Text
    deriving (Eq, Show)
