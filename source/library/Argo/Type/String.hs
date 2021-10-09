module Argo.Type.String
    ( Argo.Type.String.String(..)
    ) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Text as Text

newtype String
    = String Text.Text
    deriving (Eq, Show)

instance DeepSeq.NFData Argo.Type.String.String where
    rnf (String x) = DeepSeq.rnf x
