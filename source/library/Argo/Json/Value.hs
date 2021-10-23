{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Value where

import Control.Applicative ((<|>))

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Json.Array as Array
import qualified Argo.Json.Boolean as Boolean
import qualified Argo.Json.Null as Null
import qualified Argo.Json.Number as Number
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

data Value
    = Null Null.Null
    | Boolean Boolean.Boolean
    | Number Number.Number
    | String String.String
    | Array (Array.ArrayOf Value)
    | Object (Object.ObjectOf Value)
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: Encoder.Encoder Value
encode = Encoder.Encoder $ \ n x -> case x of
    Null y -> Encoder.run Null.encode n y
    Boolean y -> Encoder.run Boolean.encode n y
    Number y -> Encoder.run Number.encode n y
    String y -> Encoder.run String.encode n y
    Array y -> Encoder.run (Array.encode encode) n y
    Object y -> Encoder.run (Object.encode encode) n y

decode :: Decoder.Decoder Value
decode =
    Null <$> Null.decode
    <|> Boolean <$> Boolean.decode
    <|> Number <$> Number.decode
    <|> String <$> String.decode
    <|> Array <$> Array.decode decode
    <|> Object <$> Object.decode decode
