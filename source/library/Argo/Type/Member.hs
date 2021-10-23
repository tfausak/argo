{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Type.Member where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Type.Name as Name
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

data MemberOf value
    = Member Name.Name value
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: (value -> Builder.Builder) -> MemberOf value -> Builder.Builder
encode g (Member x y) =
    Name.encode x
    <> Builder.word8 Literal.colon
    <> g y

decode :: Decoder.Decoder value -> Decoder.Decoder (MemberOf value)
decode g = Member
    <$> Name.decode <* Decoder.word8 Literal.colon <* Decoder.spaces
    <*> g
