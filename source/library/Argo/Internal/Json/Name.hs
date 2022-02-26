{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Json.Name where

import qualified Argo.Internal.Json.String as String
import qualified Argo.Internal.Type.Decoder as Decoder
import qualified Argo.Internal.Type.Encoder as Encoder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Data.String
import qualified GHC.Generics as Generics

newtype Name
    = Name String.String
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Ord, Show)

instance Data.String.IsString Name where
    fromString = fromString . Data.String.fromString

fromString :: String.String -> Name
fromString = Name

toString :: Name -> String.String
toString (Name x) = x

encode :: Name -> Encoder.Encoder ()
encode = String.encode . toString

decode :: Decoder.Decoder Name
decode = fromString <$> String.decode