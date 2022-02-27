{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Json.Member where

import qualified Argo.Internal.Json.Name as Name
import qualified Argo.Internal.Literal as Literal
import qualified Argo.Internal.Type.Config as Config
import qualified Argo.Internal.Type.Decoder as Decoder
import qualified Argo.Internal.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad

data Member value = Member Name.Name value
    deriving (Eq, TH.Lift, Show)

instance DeepSeq.NFData value => DeepSeq.NFData (Member value) where
    rnf = DeepSeq.rnf . toTuple

fromTuple :: (Name.Name, value) -> Member value
fromTuple = uncurry Member

toTuple :: Member value -> (Name.Name, value)
toTuple (Member k v) = (k, v)

encode :: (value -> Encoder.Encoder ()) -> Member value -> Encoder.Encoder ()
encode f (Member x y) = do
    Name.encode x
    Trans.lift . Trans.tell $ Builder.word8 Literal.colon
    config <- Trans.ask
    Monad.when (Config.hasIndent config)
        . Trans.lift
        . Trans.tell
        $ Builder.word8 Literal.space
    f y

decode :: Decoder.Decoder value -> Decoder.Decoder (Member value)
decode g =
    Member
        <$> Name.decode
        <* Decoder.word8 Literal.colon
        <* Decoder.spaces
        <*> g
