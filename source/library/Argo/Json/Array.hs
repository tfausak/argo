{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Array where

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified GHC.Generics as Generics

newtype ArrayOf value
    = Array [value]
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: (value -> Encoder.Encoder ()) -> ArrayOf value -> Encoder.Encoder ()
encode f (Array xs) = do
    Trans.lift . Trans.tell $ Builder.word8 Literal.leftSquareBracket
    Monad.forM_ (zip (False : repeat True) xs) $ \ (p, x) -> do
        Monad.when p . Trans.lift . Trans.tell $ Builder.word8 Literal.comma
        f x
    Trans.lift . Trans.tell $ Builder.word8 Literal.rightSquareBracket

decode :: Decoder.Decoder value -> Decoder.Decoder (ArrayOf value)
decode f = do
    Decoder.word8 Literal.leftSquareBracket
    Decoder.spaces
    xs <- Decoder.list f
    Decoder.word8 Literal.rightSquareBracket
    Decoder.spaces
    pure $ Array xs
