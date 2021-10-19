{-# LANGUAGE PatternSynonyms #-}

module Argo.Pattern where

import qualified Argo.Type as Type
import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Text as Text

pattern Null :: Value.Value
pattern Null = Value.Null (Null.Null ())

pattern Boolean :: Bool -> Value.Value
pattern Boolean x = Value.Boolean (Boolean.Boolean x)

pattern Number :: Integer -> Integer -> Value.Value
pattern Number x y <- Value.Number (Number.Number x y) where
    Number x y = Value.Number $ Number.number x y

pattern String :: Text.Text -> Value.Value
pattern String x = Value.String (String.String x)

pattern Array :: Type.Array -> Value.Value
pattern Array x = Value.Array (Array.Array x)

pattern Object :: Type.Object -> Value.Value
pattern Object x = Value.Object (Object.Object x)

{-# COMPLETE Null, Boolean, Number, String, Array, Object #-}

pattern Pair :: Text.Text -> Value.Value -> Pair.Pair String.String Value.Value
pattern Pair k v = Pair.Pair (String.String k, v)

{-# COMPLETE Pair #-}
