{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Boolean where

import Control.Applicative ((<|>))

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder

newtype Boolean
    = Boolean Bool
    deriving (Eq, Show)

instance TH.Lift Boolean where
    liftTyped (Boolean x) = [|| Boolean x ||]

instance DeepSeq.NFData Boolean where
    rnf (Boolean x) = DeepSeq.rnf x

encode :: Boolean -> Builder.Builder
encode (Boolean x) =
    Builder.byteString $ if x then Literal.true else Literal.false

decode :: Decoder.Decoder Boolean
decode = decodeFalse <|> decodeTrue

decodeFalse :: Decoder.Decoder Boolean
decodeFalse = Boolean False <$ Decoder.byteString Literal.false <* Decoder.spaces

decodeTrue :: Decoder.Decoder Boolean
decodeTrue = Boolean True <$ Decoder.byteString Literal.true <* Decoder.spaces
