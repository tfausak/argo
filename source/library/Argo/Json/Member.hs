{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Member where

import qualified Argo.Json.Name as Name
import qualified Argo.Literal as Literal
import qualified Argo.Type.Config as Config
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified GHC.Generics as Generics

data MemberOf value = Member Name.Name value
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode
    :: (value -> Encoder.Encoder ()) -> MemberOf value -> Encoder.Encoder ()
encode f (Member x y) = do
    Name.encode x
    Trans.lift . Trans.tell $ Builder.word8 Literal.colon
    config <- Trans.ask
    Monad.when (Config.hasIndent config)
        . Trans.lift
        . Trans.tell
        $ Builder.word8 Literal.space
    f y

decode :: Decoder.Decoder value -> Decoder.Decoder (MemberOf value)
decode g =
    Member
        <$> Name.decode
        <* Decoder.word8 Literal.colon
        <* Decoder.spaces
        <*> g
