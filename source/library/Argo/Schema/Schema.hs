{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Schema.Schema where

import qualified Argo.Json.Value as Value
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

-- | A JSON Schema.
-- <https://json-schema.org/draft/2020-12/json-schema-core.html>
newtype Schema
    = Schema Value.Value
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

fromValue :: Value.Value -> Schema
fromValue = Schema

toValue :: Schema -> Value.Value
toValue (Schema x) = x
