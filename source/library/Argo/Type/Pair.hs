module Argo.Type.Pair where

import qualified Argo.Literal as Literal
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder

newtype Pair k v
    = Pair (k, v)
    deriving (Eq, Show)

instance (DeepSeq.NFData k, DeepSeq.NFData v) => DeepSeq.NFData (Pair k v) where
    rnf (Pair x) = DeepSeq.rnf x

encode :: (k -> Builder.Builder) -> (v -> Builder.Builder) -> Pair k v -> Builder.Builder
encode f g (Pair (x, y)) =
    f x
    <> Builder.word8 Literal.colon
    <> g y
