{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Schema.Schema where

import qualified Argo.Json.Array as Array
import qualified Argo.Json.Boolean as Boolean
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text
import qualified GHC.Generics as Generics

-- | A JSON Schema.
-- <https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-01>
newtype Schema
    = Schema Value.Value
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

instance Semigroup Schema where
    x <> y = fromValue . Value.Object $ Object.fromList
        [ Member.fromTuple
              ( Name.fromString . String.fromText $ Text.pack "oneOf"
              , Value.Array $ Array.fromList [toValue x, toValue y]
              )
        ]

instance Monoid Schema where
    mempty = true

fromValue :: Value.Value -> Schema
fromValue = Schema

toValue :: Schema -> Value.Value
toValue (Schema x) = x

false :: Schema
false = fromValue . Value.Boolean $ Boolean.fromBool False

true :: Schema
true = fromValue . Value.Boolean $ Boolean.fromBool True
