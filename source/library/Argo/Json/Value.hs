{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Value where

import Control.Applicative ((<|>))

import qualified Argo.Json.Array as Array
import qualified Argo.Json.Boolean as Boolean
import qualified Argo.Json.Null as Null
import qualified Argo.Json.Number as Number
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Type.Encoder as Encoder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

-- | A JSON (JavaScript Object Notation) value, as described by RFC 8259.
-- <https://datatracker.ietf.org/doc/html/rfc8259>
data Value
    = Null Null.Null
    | Boolean Boolean.Boolean
    | Number Number.Number
    | String String.String
    | Array (Array.ArrayOf Value)
    | Object (Object.ObjectOf Value)
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: Value -> Encoder.Encoder ()
encode x = case x of
    Null y -> Null.encode y
    Boolean y -> Boolean.encode y
    Number y -> Number.encode y
    String y -> String.encode y
    Array y -> Array.encode encode y
    Object y -> Object.encode encode y

decode :: Decoder.Decoder Value
decode =
    Null <$> Null.decode
    <|> Boolean <$> Boolean.decode
    <|> Number <$> Number.decode
    <|> String <$> String.decode
    <|> Array <$> Array.decode decode
    <|> Object <$> Object.decode decode
