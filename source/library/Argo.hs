{-# LANGUAGE PatternSynonyms #-}

module Argo
    ( Value
    , Array
    , Pair
    , Object
    , pattern Null
    , pattern Boolean
    , pattern Number
    , pattern String
    , pattern Array
    , pattern Object
    , pattern Pair
    , encode
    , decode
    , FromValue.FromValue(..)
    , ToValue.ToValue(..)
    ) where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Decoder as Decoder
import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text

type Value = Value.Value

type Array = Data.Array.Array Int Value

type Pair = Pair.Pair String.String Value

type Object = Data.Array.Array Int Pair

pattern Null :: Value
pattern Null = Value.Null (Null.Null ())

pattern Boolean :: Bool -> Value
pattern Boolean x = Value.Boolean (Boolean.Boolean x)

pattern Number :: Integer -> Integer -> Value
pattern Number x y <- Value.Number (Number.Number x y) where
    Number x y = Value.Number . Number.normalize $ Number.Number x y

pattern String :: Text.Text -> Value
pattern String x = Value.String (String.String x)

pattern Array :: Array -> Value
pattern Array x = Value.Array (Array.Array x)

pattern Object :: Object -> Value
pattern Object x = Value.Object (Object.Object x)

{-# COMPLETE Null, Boolean, Number, String, Array, Object #-}

pattern Pair :: Text.Text -> Value -> Pair
pattern Pair k v = Pair.Pair (String.String k, v)

{-# COMPLETE Pair #-}

encode :: ToValue.ToValue a => a -> Builder.Builder
encode = Value.encode . ToValue.toValue

decode :: FromValue.FromValue a => ByteString.ByteString -> Maybe a
decode x = do
    (_, y) <- Decoder.run (Decoder.spaces *> Value.decode <* Decoder.eof) x
    FromValue.fromValue y
