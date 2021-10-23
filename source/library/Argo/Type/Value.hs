{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Type.Value where

import Control.Applicative ((<|>))

import qualified Argo.Decoder as Decoder
import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.String as String
import qualified Argo.Vendor.Builder as Builder
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

encode :: Value -> Builder.Builder
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
