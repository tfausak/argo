module Argo.Type.Pair where

import qualified Control.DeepSeq as DeepSeq

newtype Pair k v
    = Pair (k, v)
    deriving (Eq, Show)

instance (DeepSeq.NFData k, DeepSeq.NFData v) => DeepSeq.NFData (Pair k v) where
    rnf (Pair x) = DeepSeq.rnf x
