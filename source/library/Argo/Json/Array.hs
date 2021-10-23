{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Array where

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Data.Semigroup as Semigroup
import qualified GHC.Generics as Generics

newtype ArrayOf value
    = Array [value]
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: Encoder.Encoder value -> Encoder.Encoder (ArrayOf value)
encode e = Encoder.Encoder $ \ n (Array xs) ->
    Builder.word8 Literal.leftSquareBracket
    <> foldMap
        (\ (p, x) -> (if p then Builder.word8 Literal.comma else mempty)
            <> Builder.word8 Literal.newLine
            <> Semigroup.stimesMonoid (n + 1) (Builder.word8 Literal.horizontalTabulation)
            <> Encoder.run e (n + 1) x)
        (zip (False : repeat True) xs)
    <> Builder.word8 Literal.newLine
    <> Semigroup.stimesMonoid n (Builder.word8 Literal.horizontalTabulation)
    <> Builder.word8 Literal.rightSquareBracket

decode :: Decoder.Decoder value -> Decoder.Decoder (ArrayOf value)
decode f = do
    Decoder.word8 Literal.leftSquareBracket
    Decoder.spaces
    xs <- Decoder.list f
    Decoder.word8 Literal.rightSquareBracket
    Decoder.spaces
    pure $ Array xs
