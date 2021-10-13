module Argo.Type.Object where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Array as Array
import qualified Data.ByteString.Builder as Builder

newtype Object a
    = Object (Array.Array Int (Pair.Pair String.String a))
    deriving (Eq, Show)

instance DeepSeq.NFData a => DeepSeq.NFData (Object a) where
    rnf (Object x) = DeepSeq.rnf x

encode :: (a -> Builder.Builder) -> Object a -> Builder.Builder
encode f (Object x) =
    Builder.word8 Literal.leftCurlyBracket
    <> foldMap
        (\ (i, e) -> (if i /= 0 then Builder.word8 Literal.comma else mempty)
            <> Pair.encode String.encode f e)
        (Array.assocs x)
    <> Builder.word8 Literal.rightCurlyBracket

decode :: Decoder.Decoder a -> Decoder.Decoder (Object a)
decode f = do
    Decoder.word8 Literal.leftCurlyBracket
    Decoder.spaces
    xs <- Decoder.array $ Pair.decode String.decode f
    Decoder.word8 Literal.rightCurlyBracket
    Decoder.spaces
    pure $ Object xs
