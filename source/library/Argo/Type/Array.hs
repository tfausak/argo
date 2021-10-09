module Argo.Type.Array
    ( Array(..)
    ) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Array as Array

newtype Array a
    = Array (Array.Array Int a)
    deriving (Eq, Show)

instance DeepSeq.NFData a => DeepSeq.NFData (Array a) where
    rnf (Array x) = DeepSeq.rnf x
