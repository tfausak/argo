{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Name where

import qualified Argo.Json.String as String
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Type.Encoder as Encoder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

newtype Name
    = Name String.String
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

fromString :: String.String -> Name
fromString = Name

toString :: Name -> String.String
toString (Name x) = x

encode :: Name -> Encoder.Encoder ()
encode = String.encode . toString

decode :: Decoder.Decoder Name
decode = fromString <$> String.decode
