module Argo.Type.Array where

import qualified Argo.Literal as Literal
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Array as Array
import qualified Data.ByteString.Builder as Builder

newtype Array a
    = Array (Array.Array Int a)
    deriving (Eq, Show)

instance DeepSeq.NFData a => DeepSeq.NFData (Array a) where
    rnf (Array x) = DeepSeq.rnf x

encode :: (a -> Builder.Builder) -> Array a -> Builder.Builder
encode f (Array x) =
    Builder.word8 Literal.leftSquareBracket
    <> foldMap
        (\ (i, e) -> (if i /= 0 then Builder.word8 Literal.comma else mempty)
            <> f e)
        (Array.assocs x)
    <> Builder.word8 Literal.rightSquareBracket
