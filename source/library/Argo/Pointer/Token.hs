{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Pointer.Token where

import qualified Argo.Literal as Literal
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.ByteString as ByteString
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.String as String
import qualified Data.Word as Word
import qualified GHC.Generics as Generics

newtype Token
    = Token Text.Text
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

instance String.IsString Token where
    fromString = fromText . String.fromString

fromText :: Text.Text -> Token
fromText = Token

toText :: Token -> Text.Text
toText (Token x) = x

decode :: Decoder.Decoder Token
decode = do
    x <- Decoder.takeWhile $ (/=) Literal.solidus
    y <- either (Trans.lift . Trans.throwE) pure $ unescape x
    case Text.decodeUtf8' y of
        Left e -> Trans.lift . Trans.throwE $ show e
        Right z -> pure $ fromText z

unescape :: ByteString.ByteString -> Either String ByteString.ByteString
unescape = fmap ByteString.pack . unescapeHelper . ByteString.unpack

unescapeHelper :: [Word.Word8] -> Either String [Word.Word8]
unescapeHelper xs = case xs of
    [] -> pure xs
    x : ys -> if x == Literal.tilde
        then case ys of
            y : zs
                | y == Literal.digitZero -> (:) Literal.tilde <$> unescapeHelper zs
                | y == Literal.digitOne -> (:) Literal.solidus <$> unescapeHelper zs
            _ -> Left "invalid escape"
        else (:) x <$> unescapeHelper ys

encode :: Token -> Encoder.Encoder ()
encode = Trans.lift
    . Trans.tell
    . Text.encodeUtf8BuilderEscaped encodeChar
    . toText

encodeChar :: Builder.BoundedPrim Word.Word8
encodeChar =
    Builder.condB (== Literal.tilde) (encodeEscape Literal.digitZero)
    . Builder.condB (== Literal.solidus) (encodeEscape Literal.digitOne)
    $ Builder.liftFixedToBounded Builder.word8F

encodeEscape :: Word.Word8 -> Builder.BoundedPrim a
encodeEscape x = Builder.liftFixedToBounded
    $ const (Literal.tilde, x)
    Builder.>$< Builder.word8F
    Builder.>*< Builder.word8F
