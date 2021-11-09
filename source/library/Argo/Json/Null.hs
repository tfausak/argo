{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Null where

import qualified Argo.Literal as Literal
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans
import qualified GHC.Generics as Generics

newtype Null
    = Null ()
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

fromUnit :: () -> Null
fromUnit = Null

toUnit :: Null -> ()
toUnit (Null x) = x

encode :: Null -> Encoder.Encoder ()
encode = const . Trans.lift . Trans.tell $ Builder.byteString Literal.null

decode :: Decoder.Decoder Null
decode = fromUnit () <$ Decoder.byteString Literal.null <* Decoder.spaces
