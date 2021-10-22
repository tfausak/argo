{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Null where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Data.ByteString.Builder as Builder

newtype Null
    = Null ()
    deriving (Eq, Show)

instance TH.Lift Null where
    liftTyped (Null x) = [|| Null x ||]

instance DeepSeq.NFData Null where
    rnf (Null x) = DeepSeq.rnf x

encode :: Null -> Builder.Builder
encode = const $ Builder.byteString Literal.null

decode :: Decoder.Decoder Null
decode = Null () <$ Decoder.byteString Literal.null <* Decoder.spaces
