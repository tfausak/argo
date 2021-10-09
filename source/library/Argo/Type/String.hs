module Argo.Type.String
    ( Argo.Type.String.String(..)
    ) where

import qualified Data.Text as Text

newtype String
    = String Text.Text
    deriving (Eq, Show)
