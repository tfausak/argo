module Argo.Type.String where

import qualified Argo.Literal as Literal
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word

newtype String
    = String Text.Text
    deriving (Eq, Show)

instance DeepSeq.NFData Argo.Type.String.String where
    rnf (String x) = DeepSeq.rnf x

encode :: Argo.Type.String.String -> Builder.Builder
encode (String x) =
    Builder.word8 Literal.quotationMark
    <> Text.encodeUtf8BuilderEscaped encodeChar x
    <> Builder.word8 Literal.quotationMark

encodeChar :: P.BoundedPrim Word.Word8
encodeChar =
    P.condB (== Literal.quotationMark) (encodeShortEscape Literal.quotationMark)
    . P.condB (== Literal.reverseSolidus) (encodeShortEscape Literal.reverseSolidus)
    . P.condB (== Literal.backspace) (encodeShortEscape Literal.latinSmallLetterB)
    . P.condB (== Literal.formFeed) (encodeShortEscape Literal.latinSmallLetterF)
    . P.condB (== Literal.newLine) (encodeShortEscape Literal.latinSmallLetterN)
    . P.condB (== Literal.carriageReturn) (encodeShortEscape Literal.latinSmallLetterR)
    . P.condB (== Literal.horizontalTabulation) (encodeShortEscape Literal.latinSmallLetterT)
    . P.condB (< Literal.space) encodeLongEscape
    $ P.liftFixedToBounded P.word8

encodeShortEscape :: Word.Word8 -> P.BoundedPrim a
encodeShortEscape x = P.liftFixedToBounded
    $ const (Literal.reverseSolidus, x)
    P.>$< P.word8
    P.>*< P.word8

encodeLongEscape :: P.BoundedPrim Word.Word8
encodeLongEscape = P.liftFixedToBounded
    $ (\ x -> (Literal.reverseSolidus, (Literal.latinSmallLetterU, word8ToWord16 x)))
    P.>$< P.word8
    P.>*< P.word8
    P.>*< P.word16HexFixed

word8ToWord16 :: Word.Word8 -> Word.Word16
word8ToWord16 = fromIntegral
