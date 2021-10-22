{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Name where

import qualified Argo.Decoder as Decoder
import qualified Argo.Type.String as String
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH

newtype Name
    = Name String.String
    deriving (Eq, Show)

instance TH.Lift Name where
    liftTyped (Name x) = [|| Name x ||]

instance DeepSeq.NFData Name where
    rnf (Name x) = DeepSeq.rnf x

encode :: Name -> Builder.Builder
encode (Name x) = String.encode x

decode :: Decoder.Decoder Name
decode = Name <$> String.decode
