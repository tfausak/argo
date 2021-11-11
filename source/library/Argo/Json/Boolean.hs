{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Boolean where

import Control.Applicative ((<|>))

import qualified Argo.Literal as Literal
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans
import qualified GHC.Generics as Generics

newtype Boolean
    = Boolean Bool
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

fromBool :: Bool -> Boolean
fromBool = Boolean

toBool :: Boolean -> Bool
toBool (Boolean x) = x

encode :: Boolean -> Encoder.Encoder ()
encode x = Trans.lift . Trans.tell . Builder.byteString $ if toBool x
    then Literal.true
    else Literal.false

decode :: Decoder.Decoder Boolean
decode = decodeFalse <|> decodeTrue

decodeFalse :: Decoder.Decoder Boolean
decodeFalse =
    fromBool False <$ Decoder.byteString Literal.false <* Decoder.spaces

decodeTrue :: Decoder.Decoder Boolean
decodeTrue =
    fromBool True <$ Decoder.byteString Literal.true <* Decoder.spaces
