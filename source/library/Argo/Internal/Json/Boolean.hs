{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Json.Boolean where

import Control.Applicative ((<|>))

import qualified Argo.Internal.Literal as Literal
import qualified Argo.Internal.Type.Decoder as Decoder
import qualified Argo.Internal.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans

newtype Boolean
    = Boolean Bool
    deriving (Eq, TH.Lift, Show)

instance DeepSeq.NFData Boolean where
    rnf = DeepSeq.rnf . toBool

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
