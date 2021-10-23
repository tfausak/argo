{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Type.Array where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

newtype ArrayOf value
    = Array [value]
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: (value -> Builder.Builder) -> ArrayOf value -> Builder.Builder
encode f (Array x) =
    Builder.word8 Literal.leftSquareBracket
    <> foldMap
        (\ (p, e) -> (if p then Builder.word8 Literal.comma else mempty)
            <> f e)
        (zip (False : repeat True) x)
    <> Builder.word8 Literal.rightSquareBracket

decode :: Decoder.Decoder value -> Decoder.Decoder (ArrayOf value)
decode f = do
    Decoder.word8 Literal.leftSquareBracket
    Decoder.spaces
    xs <- Decoder.list f
    Decoder.word8 Literal.rightSquareBracket
    Decoder.spaces
    pure $ Array xs
