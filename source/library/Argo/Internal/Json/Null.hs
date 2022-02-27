{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Json.Null where

import qualified Argo.Internal.Literal as Literal
import qualified Argo.Internal.Type.Decoder as Decoder
import qualified Argo.Internal.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans

newtype Null
    = Null ()
    deriving (Eq, TH.Lift, Show)

instance DeepSeq.NFData Null where
    rnf = DeepSeq.rnf . toUnit

fromUnit :: () -> Null
fromUnit = Null

toUnit :: Null -> ()
toUnit (Null x) = x

encode :: Null -> Encoder.Encoder ()
encode = const . Trans.lift . Trans.tell $ Builder.byteString Literal.null

decode :: Decoder.Decoder Null
decode = fromUnit () <$ Decoder.byteString Literal.null <* Decoder.spaces
