{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Object where

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Json.Member as Member
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Data.Semigroup as Semigroup
import qualified GHC.Generics as Generics

newtype ObjectOf value
    = Object [Member.MemberOf value]
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: Encoder.Encoder value -> Encoder.Encoder (ObjectOf value)
encode e = Encoder.Encoder $ \ n (Object xs) ->
    Builder.word8 Literal.leftCurlyBracket
    <> foldMap
        (\ (p, x) -> (if p then Builder.word8 Literal.comma else mempty)
            <> Builder.word8 Literal.newLine
            <> Semigroup.stimesMonoid (n + 1) (Builder.word8 Literal.horizontalTabulation)
            <> Encoder.run (Member.encode e) (n + 1) x)
        (zip (False : repeat True) xs)
    <> Builder.word8 Literal.newLine
    <> Semigroup.stimesMonoid n (Builder.word8 Literal.horizontalTabulation)
    <> Builder.word8 Literal.rightCurlyBracket

decode :: Decoder.Decoder value -> Decoder.Decoder (ObjectOf value)
decode f = do
    Decoder.word8 Literal.leftCurlyBracket
    Decoder.spaces
    xs <- Decoder.list $ Member.decode f
    Decoder.word8 Literal.rightCurlyBracket
    Decoder.spaces
    pure $ Object xs
