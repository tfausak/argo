{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Object where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Type.Member as Member
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH

newtype Object a
    = Object [Member.Member a]
    deriving (Eq, Show)

instance TH.Lift a => TH.Lift (Object a) where
    liftTyped (Object x) = [|| Object x ||]

instance DeepSeq.NFData a => DeepSeq.NFData (Object a) where
    rnf (Object x) = DeepSeq.rnf x

encode :: (a -> Builder.Builder) -> Object a -> Builder.Builder
encode f (Object x) =
    Builder.word8 Literal.leftCurlyBracket
    <> foldMap
        (\ (i, e) -> (if i /= 0 then Builder.word8 Literal.comma else mempty)
            <> Member.encode f e)
        (zip [ 0 :: Int .. ] x)
    <> Builder.word8 Literal.rightCurlyBracket

decode :: Decoder.Decoder a -> Decoder.Decoder (Object a)
decode f = do
    Decoder.word8 Literal.leftCurlyBracket
    Decoder.spaces
    xs <- Decoder.array $ Member.decode f
    Decoder.word8 Literal.rightCurlyBracket
    Decoder.spaces
    pure $ Object xs
