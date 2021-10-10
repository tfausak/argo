module Argo.Type.Null where

import qualified Control.DeepSeq as DeepSeq

newtype Null
    = Null ()
    deriving (Eq, Show)

instance DeepSeq.NFData Null where
    rnf (Null x) = DeepSeq.rnf x
