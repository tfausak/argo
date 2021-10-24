{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.String where

import qualified Argo.Decoder as Decoder
import qualified Argo.Encoder as Encoder
import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.ByteString as ByteString
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.Word as Word
import qualified GHC.Generics as Generics

newtype String
    = String Text.Text
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

encode :: Argo.Json.String.String -> Encoder.Encoder ()
encode (String x) = Trans.lift
    . Trans.tell
    $ Builder.word8 Literal.quotationMark
    <> Text.encodeUtf8BuilderEscaped encodeChar x
    <> Builder.word8 Literal.quotationMark

encodeChar :: Builder.BoundedPrim Word.Word8
encodeChar =
    Builder.condB (== Literal.quotationMark) (encodeShortEscape Literal.quotationMark)
    . Builder.condB (== Literal.reverseSolidus) (encodeShortEscape Literal.reverseSolidus)
    . Builder.condB (== Literal.backspace) (encodeShortEscape Literal.latinSmallLetterB)
    . Builder.condB (== Literal.formFeed) (encodeShortEscape Literal.latinSmallLetterF)
    . Builder.condB (== Literal.newLine) (encodeShortEscape Literal.latinSmallLetterN)
    . Builder.condB (== Literal.carriageReturn) (encodeShortEscape Literal.latinSmallLetterR)
    . Builder.condB (== Literal.horizontalTabulation) (encodeShortEscape Literal.latinSmallLetterT)
    . Builder.condB (< Literal.space) encodeLongEscape
    $ Builder.liftFixedToBounded Builder.word8F

encodeShortEscape :: Word.Word8 -> Builder.BoundedPrim a
encodeShortEscape x = Builder.liftFixedToBounded
    $ const (Literal.reverseSolidus, x)
    Builder.>$< Builder.word8F
    Builder.>*< Builder.word8F

encodeLongEscape :: Builder.BoundedPrim Word.Word8
encodeLongEscape = Builder.liftFixedToBounded
    $ (\ x -> (Literal.reverseSolidus, (Literal.latinSmallLetterU, word8ToWord16 x)))
    Builder.>$< Builder.word8F
    Builder.>*< Builder.word8F
    Builder.>*< Builder.word16HexFixed

word8ToWord16 :: Word.Word8 -> Word.Word16
word8ToWord16 = fromIntegral

decode :: Decoder.Decoder Argo.Json.String.String
decode = do
    Decoder.word8 Literal.quotationMark
    b1 <- Decoder.get
    i <- case getClose b1 0 of
        Nothing -> fail "unterminated string"
        Just i -> pure i
    let (xs, b2) = ByteString.splitAt i b1
    Monad.when (ByteString.any (< Literal.space) xs) $ fail "unescaped control character"
    Decoder.put b2
    Decoder.word8 Literal.quotationMark
    Decoder.spaces
    case Text.decodeUtf8' xs of
        Left e -> fail $ show e
        Right x -> case unescapeText x of
            Nothing -> fail "invalid escape"
            Just y -> pure $ String y

findAt :: Word.Word8 -> Int -> ByteString.ByteString -> Maybe Int
findAt x i = fmap (+ i) . ByteString.elemIndex x . ByteString.drop i

countConsecutive :: Word.Word8 -> Int -> ByteString.ByteString -> Int
countConsecutive x i = ByteString.length . ByteString.takeWhileEnd (== x) . ByteString.take i

getClose :: ByteString.ByteString -> Int -> Maybe Int
getClose b i = do
    j <- findAt Literal.quotationMark i b
    let n = countConsecutive Literal.reverseSolidus j b
    if even n then Just j else getClose b $ j + 1

unescapeText :: Text.Text -> Maybe Text.Text
unescapeText = fmap (Text.pack . combineSurrogatePairs) . unescapeString . Text.unpack

combineSurrogatePairs :: Prelude.String -> Prelude.String
combineSurrogatePairs xs = case xs of
    "" -> xs
    x : y : zs | isHighSurrogate x && isLowSurrogate y ->
        combineSurrogatePair x y : combineSurrogatePairs zs
    x : ys -> x : combineSurrogatePairs ys

combineSurrogatePair :: Char -> Char -> Char
combineSurrogatePair hi lo = Char.chr
    $ 0x10000
    + ((Char.ord hi - 0xd800) * 0x400)
    + (Char.ord lo - 0xdc00)

isHighSurrogate :: Char -> Bool
isHighSurrogate x = '\xd800' <= x && x <= '\xdbff'

isLowSurrogate :: Char -> Bool
isLowSurrogate x = '\xdc00' <= x && x <= '\xdfff'

unescapeString :: Prelude.String -> Maybe Prelude.String
unescapeString xs = case xs of
    "" -> pure xs
    '\\' : ys -> case ys of
        "" -> fail "empty escape"
        x : zs -> case x of
            '"' -> ('"' :) <$> unescapeString zs
            '\\' -> ('\\' :) <$> unescapeString zs
            '/' -> ('/' :) <$> unescapeString zs
            'b' -> ('\b' :) <$> unescapeString zs
            'f' -> ('\f' :) <$> unescapeString zs
            'n' -> ('\n' :) <$> unescapeString zs
            'r' -> ('\r' :) <$> unescapeString zs
            't' -> ('\t' :) <$> unescapeString zs
            'u' -> case zs of
                a : b : c : d : es | Just y <- fromLongEscape a b c d ->
                    (y :) <$> unescapeString es
                _ -> fail "invalid long escape"
            _ -> fail "invalid short escape"
    x : ys -> (x :) <$> unescapeString ys

fromLongEscape :: Char -> Char -> Char -> Char -> Maybe Char
fromLongEscape a b c d = do
    w <- fromHexadecimalDigit a
    x <- fromHexadecimalDigit b
    y <- fromHexadecimalDigit c
    z <- fromHexadecimalDigit d
    pure . Char.chr $ (0x1000 * w) + (0x100 * x) + (0x10 * y) + z

fromHexadecimalDigit :: Char -> Maybe Int
fromHexadecimalDigit x =
    if Char.isHexDigit x then Just $ Char.digitToInt x else Nothing
