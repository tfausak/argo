{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Member where

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Json.Name as Name
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

data MemberOf value
    = Member Name.Name value
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: Encoder.Encoder value -> Encoder.Encoder (MemberOf value)
encode e = Encoder.Encoder $ \ n (Member x y) ->
    Encoder.run Name.encode n x
    <> Builder.word8 Literal.colon
    <> Builder.word8 Literal.space
    <> Encoder.run e n y

decode :: Decoder.Decoder value -> Decoder.Decoder (MemberOf value)
decode g = Member
    <$> Name.decode <* Decoder.word8 Literal.colon <* Decoder.spaces
    <*> g
