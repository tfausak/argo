module Argo.Type.Boolean
    ( Boolean(..)
    ) where

import qualified Control.DeepSeq as DeepSeq

newtype Boolean
    = Boolean Bool
    deriving (Eq, Show)

instance DeepSeq.NFData Boolean where
    rnf (Boolean x) = DeepSeq.rnf x
