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
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified GHC.Generics as Generics

newtype ObjectOf value
    = Object [Member.MemberOf value]
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: (value -> Encoder.Encoder ()) -> ObjectOf value -> Encoder.Encoder ()
encode f (Object xs) = Encoder.list
    (Trans.lift . Trans.tell $ Builder.word8 Literal.leftCurlyBracket)
    (Trans.lift . Trans.tell $ Builder.word8 Literal.rightCurlyBracket)
    (Trans.lift . Trans.tell $ Builder.word8 Literal.comma)
    (Member.encode f)
    xs

encodeElement :: (value -> Encoder.Encoder ()) -> Int -> Member.MemberOf value -> Encoder.Encoder ()
encodeElement f i x = do
    Monad.when (i > 0) . Trans.lift . Trans.tell $ Builder.word8 Literal.comma
    Member.encode f x

decode :: Decoder.Decoder value -> Decoder.Decoder (ObjectOf value)
decode f = do
    Decoder.word8 Literal.leftCurlyBracket
    Decoder.spaces
    xs <- Decoder.list $ Member.decode f
    Decoder.word8 Literal.rightCurlyBracket
    Decoder.spaces
    pure $ Object xs
