{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Type.Name where

import qualified Argo.Decoder as Decoder
import qualified Argo.Type.String as String
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

newtype Name
    = Name String.String
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: Name -> Builder.Builder
encode (Name x) = String.encode x

decode :: Decoder.Decoder Name
decode = Name <$> String.decode
