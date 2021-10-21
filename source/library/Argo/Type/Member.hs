{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Member where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder
import qualified Language.Haskell.TH.Syntax as TH

data Member k v
    = Member k v
    deriving (Eq, Show)

instance (TH.Lift k, TH.Lift v) => TH.Lift (Member k v) where
    liftTyped (Member x y) = [|| Member x y ||]

instance (DeepSeq.NFData k, DeepSeq.NFData v) => DeepSeq.NFData (Member k v) where
    rnf (Member x y) = DeepSeq.deepseq x $ DeepSeq.rnf y

encode :: (k -> Builder.Builder) -> (v -> Builder.Builder) -> Member k v -> Builder.Builder
encode f g (Member x y) =
    f x
    <> Builder.word8 Literal.colon
    <> g y

decode :: Decoder.Decoder k -> Decoder.Decoder v -> Decoder.Decoder (Member k v)
decode f g = Member
    <$> f <* Decoder.word8 Literal.colon <* Decoder.spaces
    <*> g
