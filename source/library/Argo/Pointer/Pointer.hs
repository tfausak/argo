{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Argo.Pointer.Pointer where

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Literal as Literal
import qualified Argo.Pointer.Token as Token
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Applicative as Applicative
import qualified GHC.Generics as Generics

-- | A JSON pointer, as described by RFC 6901.
-- <https://datatracker.ietf.org/doc/html/rfc6901>
newtype Pointer
    = Pointer [Token.Token]
    deriving (Eq, Generics.Generic, DeepSeq.NFData, Show)

fromList :: [Token.Token] -> Pointer
fromList = Pointer

toList :: Pointer -> [Token.Token]
toList (Pointer x) = x

decode :: Decoder.Decoder Pointer
decode = fromList <$> Applicative.many decodeToken

decodeToken :: Decoder.Decoder Token.Token
decodeToken = do
    Decoder.word8 Literal.solidus
    Token.decode

encode :: Pointer -> Encoder.Encoder ()
encode = mapM_ encodeToken . toList

encodeToken :: Token.Token -> Encoder.Encoder ()
encodeToken x = do
    Trans.lift . Trans.tell $ Builder.word8 Literal.solidus
    Token.encode x
