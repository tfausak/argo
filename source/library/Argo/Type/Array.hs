{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Array where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH

newtype Array a
    = Array [a]
    deriving (Eq, Show)

instance TH.Lift a => TH.Lift (Array a) where
    liftTyped (Array x) = [|| Array x ||]

instance DeepSeq.NFData a => DeepSeq.NFData (Array a) where
    rnf (Array x) = DeepSeq.rnf x

encode :: (a -> Builder.Builder) -> Array a -> Builder.Builder
encode f (Array x) =
    Builder.word8 Literal.leftSquareBracket
    <> foldMap
        (\ (p, e) -> (if p then Builder.word8 Literal.comma else mempty)
            <> f e)
        (zip (False : repeat True) x)
    <> Builder.word8 Literal.rightSquareBracket

decode :: Decoder.Decoder a -> Decoder.Decoder (Array a)
decode f = do
    Decoder.word8 Literal.leftSquareBracket
    Decoder.spaces
    xs <- Decoder.list f
    Decoder.word8 Literal.rightSquareBracket
    Decoder.spaces
    pure $ Array xs
