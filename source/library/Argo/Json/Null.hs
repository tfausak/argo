{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Null where

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

newtype Null
    = Null ()
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: Encoder.Encoder Null
encode = Encoder.Encoder $ \ _ _ ->
    Builder.byteString Literal.null

decode :: Decoder.Decoder Null
decode = Null () <$ Decoder.byteString Literal.null <* Decoder.spaces
