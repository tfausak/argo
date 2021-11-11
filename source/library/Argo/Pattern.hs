{-# LANGUAGE PatternSynonyms #-}

module Argo.Pattern where

import qualified Argo.Json.Array as Array
import qualified Argo.Json.Boolean as Boolean
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Null as Null
import qualified Argo.Json.Number as Number
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Type.Decimal as Decimal
import qualified Argo.Vendor.Text as Text

pattern Null :: Value.Value
pattern Null = Value.Null (Null.Null ())

pattern Boolean :: Bool -> Value.Value
pattern Boolean x = Value.Boolean (Boolean.Boolean x)

pattern Number :: Decimal.Decimal -> Value.Value
pattern Number x = Value.Number (Number.Number x)

pattern String :: Text.Text -> Value.Value
pattern String x = Value.String (String.String x)

pattern Array :: [Value.Value] -> Value.Value
pattern Array x = Value.Array (Array.Array x)

pattern Object :: [Member.MemberOf Value.Value] -> Value.Value
pattern Object x = Value.Object (Object.Object x)

{-# COMPLETE Null, Boolean, Number, String, Array, Object #-}

pattern Name :: Text.Text -> Name.Name
pattern Name x = Name.Name (String.String x)

{-# COMPLETE Name #-}

pattern Decimal :: Integer -> Integer -> Decimal.Decimal
pattern Decimal s e <- Decimal.Decimal s e where
    Decimal s e = Decimal.decimal s e

{-# COMPLETE Decimal #-}
