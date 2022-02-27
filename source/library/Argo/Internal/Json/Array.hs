{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Json.Array where

import qualified Argo.Internal.Literal as Literal
import qualified Argo.Internal.Type.Decoder as Decoder
import qualified Argo.Internal.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans

newtype Array value
    = Array [value]
    deriving (Eq, TH.Lift, Show)

instance DeepSeq.NFData value => DeepSeq.NFData (Array value) where
    rnf = DeepSeq.rnf . toList

fromList :: [value] -> Array value
fromList = Array

toList :: Array value -> [value]
toList (Array x) = x

encode :: (value -> Encoder.Encoder ()) -> Array value -> Encoder.Encoder ()
encode f =
    Encoder.list
            (Trans.lift . Trans.tell $ Builder.word8 Literal.leftSquareBracket)
            (Trans.lift . Trans.tell $ Builder.word8 Literal.rightSquareBracket
            )
            (Trans.lift . Trans.tell $ Builder.word8 Literal.comma)
            f
        . toList

decode :: Decoder.Decoder value -> Decoder.Decoder (Array value)
decode f = do
    Decoder.word8 Literal.leftSquareBracket
    Decoder.spaces
    xs <- Decoder.list f
    Decoder.word8 Literal.rightSquareBracket
    Decoder.spaces
    pure $ fromList xs
