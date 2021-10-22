{-# LANGUAGE TemplateHaskellQuotes #-}

module Argo.Type.Member where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Type.Name as Name
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder

data Member v
    = Member Name.Name v
    deriving (Eq, Show)

instance TH.Lift v => TH.Lift (Member v) where
    liftTyped (Member x y) = [|| Member x y ||]

instance DeepSeq.NFData v => DeepSeq.NFData (Member v) where
    rnf (Member x y) = DeepSeq.deepseq x $ DeepSeq.rnf y

encode :: (v -> Builder.Builder) -> Member v -> Builder.Builder
encode g (Member x y) =
    Name.encode x
    <> Builder.word8 Literal.colon
    <> g y

decode :: Decoder.Decoder v -> Decoder.Decoder (Member v)
decode g = Member
    <$> Name.decode <* Decoder.word8 Literal.colon <* Decoder.spaces
    <*> g
