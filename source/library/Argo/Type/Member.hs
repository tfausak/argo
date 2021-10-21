{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Member where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder
import qualified Language.Haskell.TH.Syntax as TH

newtype Member k v
    = Member (k, v)
    deriving (Eq, Show)

instance (TH.Lift k, TH.Lift v) => TH.Lift (Member k v) where
    liftTyped (Member x) = [|| Member x ||]

instance (DeepSeq.NFData k, DeepSeq.NFData v) => DeepSeq.NFData (Member k v) where
    rnf (Member x) = DeepSeq.rnf x

encode :: (k -> Builder.Builder) -> (v -> Builder.Builder) -> Member k v -> Builder.Builder
encode f g (Member (x, y)) =
    f x
    <> Builder.word8 Literal.colon
    <> g y

decode :: Decoder.Decoder k -> Decoder.Decoder v -> Decoder.Decoder (Member k v)
decode f g = do
    k <- f
    Decoder.word8 Literal.colon
    Decoder.spaces
    v <- g
    pure $ Member (k, v)
