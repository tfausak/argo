{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Pair where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder
import qualified Language.Haskell.TH.Syntax as TH

newtype Pair k v
    = Pair (k, v)
    deriving (Eq, Show)

instance (TH.Lift k, TH.Lift v) => TH.Lift (Pair k v) where
    liftTyped (Pair x) = [|| Pair x ||]

instance (DeepSeq.NFData k, DeepSeq.NFData v) => DeepSeq.NFData (Pair k v) where
    rnf (Pair x) = DeepSeq.rnf x

encode :: (k -> Builder.Builder) -> (v -> Builder.Builder) -> Pair k v -> Builder.Builder
encode f g (Pair (x, y)) =
    f x
    <> Builder.word8 Literal.colon
    <> g y

decode :: Decoder.Decoder k -> Decoder.Decoder v -> Decoder.Decoder (Pair k v)
decode f g = do
    k <- f
    Decoder.word8 Literal.colon
    Decoder.spaces
    v <- g
    pure $ Pair (k, v)
