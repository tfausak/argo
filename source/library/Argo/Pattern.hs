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
import qualified Argo.Vendor.Text as Text

pattern Null :: Value.Value
pattern Null = Value.Null (Null.Null ())

pattern Boolean :: Bool -> Value.Value
pattern Boolean x = Value.Boolean (Boolean.Boolean x)

pattern Number :: Integer -> Integer -> Value.Value
pattern Number x y <- Value.Number (Number.Number x y) where
    Number x y = Value.Number $ Number.number x y

pattern String :: Text.Text -> Value.Value
pattern String x = Value.String (String.String x)

type Array = [Value.Value]

pattern Array :: Array -> Value.Value
pattern Array x = Value.Array (Array.Array x)

type Object = [Member]

pattern Object :: Object -> Value.Value
pattern Object x = Value.Object (Object.Object x)

{-# COMPLETE Null, Boolean, Number, String, Array, Object #-}

type Member = Member.MemberOf Value.Value

pattern Member :: Name.Name -> Value.Value -> Member
pattern Member k v = Member.Member k v

{-# COMPLETE Member #-}

pattern Name :: Text.Text -> Name.Name
pattern Name x = Name.Name (String.String x)

{-# COMPLETE Name #-}
