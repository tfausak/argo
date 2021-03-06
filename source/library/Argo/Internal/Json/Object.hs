{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Json.Object where

import qualified Argo.Internal.Json.Member as Member
import qualified Argo.Internal.Literal as Literal
import qualified Argo.Internal.Type.Decoder as Decoder
import qualified Argo.Internal.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad

newtype Object value
    = Object [Member.Member value]
    deriving (Eq, TH.Lift, Show)

instance DeepSeq.NFData value => DeepSeq.NFData (Object value) where
    rnf = DeepSeq.rnf . toList

fromList :: [Member.Member value] -> Object value
fromList = Object

toList :: Object value -> [Member.Member value]
toList (Object x) = x

encode :: (value -> Encoder.Encoder ()) -> Object value -> Encoder.Encoder ()
encode f =
    Encoder.list
            (Trans.lift . Trans.tell $ Builder.word8 Literal.leftCurlyBracket)
            (Trans.lift . Trans.tell $ Builder.word8 Literal.rightCurlyBracket)
            (Trans.lift . Trans.tell $ Builder.word8 Literal.comma)
            (Member.encode f)
        . toList

encodeElement
    :: (value -> Encoder.Encoder ())
    -> Int
    -> Member.Member value
    -> Encoder.Encoder ()
encodeElement f i x = do
    Monad.when (i > 0) . Trans.lift . Trans.tell $ Builder.word8 Literal.comma
    Member.encode f x

decode :: Decoder.Decoder value -> Decoder.Decoder (Object value)
decode f = do
    Decoder.word8 Literal.leftCurlyBracket
    Decoder.spaces
    xs <- Decoder.list $ Member.decode f
    Decoder.word8 Literal.rightCurlyBracket
    Decoder.spaces
    pure $ fromList xs
