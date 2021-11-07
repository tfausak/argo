{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Argo.Pointer.Token where

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.Word as Word
import qualified GHC.Generics as Generics

newtype Token
    = Token Text.Text
    deriving (Eq, Generics.Generic, DeepSeq.NFData, Show)

fromText :: Text.Text -> Token
fromText = Token

toText :: Token -> Text.Text
toText (Token x) = x

decode :: Decoder.Decoder Token
decode = do
    x <- Decoder.takeWhile $ (/=) Literal.solidus
    y <- case Text.decodeUtf8' x of
        Left e -> fail $ show e
        Right y -> pure y
    case unescapeText y of
        Nothing -> fail "invalid escape"
        Just z -> pure $ fromText z

unescapeText :: Text.Text -> Maybe Text.Text
unescapeText = fmap Text.pack . unescapeString . Text.unpack

unescapeString :: String -> Maybe String
unescapeString xs = case xs of
    "" -> pure xs
    x : ys -> case x of
        '~' -> case ys of
            '0' : zs -> ('~' :) <$> unescapeString zs
            '1' : zs -> ('/' :) <$> unescapeString zs
            _ -> fail "invalid escape"
        _ -> (x :) <$> unescapeString ys

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
