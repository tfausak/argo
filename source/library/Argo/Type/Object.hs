{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Type.Object where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Type.Member as Member
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

newtype ObjectOf value
    = Object [Member.MemberOf value]
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: (value -> Builder.Builder) -> ObjectOf value -> Builder.Builder
encode f (Object x) =
    Builder.word8 Literal.leftCurlyBracket
    <> foldMap
        (\ (p, e) -> (if p then Builder.word8 Literal.comma else mempty)
            <> Member.encode f e)
        (zip (False : repeat True) x)
    <> Builder.word8 Literal.rightCurlyBracket

decode :: Decoder.Decoder value -> Decoder.Decoder (ObjectOf value)
decode f = do
    Decoder.word8 Literal.leftCurlyBracket
    Decoder.spaces
    xs <- Decoder.list $ Member.decode f
    Decoder.word8 Literal.rightCurlyBracket
    Decoder.spaces
    pure $ Object xs
