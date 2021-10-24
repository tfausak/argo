{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Boolean where

import Control.Applicative ((<|>))

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans
import qualified GHC.Generics as Generics

newtype Boolean
    = Boolean Bool
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: Boolean -> Encoder.Encoder ()
encode (Boolean x) = do
    Trans.lift . Trans.tell . Builder.byteString $ if x then Literal.true else Literal.false

decode :: Decoder.Decoder Boolean
decode = decodeFalse <|> decodeTrue

decodeFalse :: Decoder.Decoder Boolean
decodeFalse = Boolean False <$ Decoder.byteString Literal.false <* Decoder.spaces

decodeTrue :: Decoder.Decoder Boolean
decodeTrue = Boolean True <$ Decoder.byteString Literal.true <* Decoder.spaces
