{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Array where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Data.Array as Array
import qualified Data.ByteString.Builder as Builder

newtype Array a
    = Array (Array.Array Int a)
    deriving (Eq, Show)

instance TH.Lift a => TH.Lift (Array a) where
    liftTyped (Array x) =
        let
            bounds = Array.bounds x
            elems = Array.elems x
        in [|| Array $ Array.listArray bounds elems ||]

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

decode :: Decoder.Decoder a -> Decoder.Decoder (Array a)
decode f = do
    Decoder.word8 Literal.leftSquareBracket
    Decoder.spaces
    xs <- Decoder.array f
    Decoder.word8 Literal.rightSquareBracket
    Decoder.spaces
    pure $ Array xs
