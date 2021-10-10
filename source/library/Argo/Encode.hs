module Argo.Encode where

import qualified Argo.Literal as Literal
import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word

encodeValue :: Value.Value -> Builder.Builder
encodeValue x = case x of
    Value.Null y -> encodeNull y
    Value.Boolean y -> encodeBoolean y
    Value.Number y -> encodeNumber y
    Value.String y -> encodeString y
    Value.Array y -> encodeArray encodeValue y
    Value.Object y -> encodeObject encodeValue y

encodeNull :: Null.Null -> Builder.Builder
encodeNull _ =
    Builder.byteString Literal.null

encodeBoolean :: Boolean.Boolean -> Builder.Builder
encodeBoolean (Boolean.Boolean x) =
    Builder.byteString $ if x then Literal.true else Literal.false

encodeNumber :: Number.Number -> Builder.Builder
encodeNumber (Number.Number x y) =
    if y == 0
    then Builder.integerDec x
    else Builder.integerDec x
        <> Builder.word8 Literal.latinSmallLetterE
        <> Builder.integerDec y

encodeString :: String.String -> Builder.Builder
encodeString (String.String x) =
    Builder.word8 Literal.quotationMark
    <> Text.encodeUtf8BuilderEscaped encodeChar x
    <> Builder.word8 Literal.quotationMark

encodeArray :: (a -> Builder.Builder) -> Array.Array a -> Builder.Builder
encodeArray f (Array.Array x) =
    Builder.word8 Literal.leftSquareBracket
    <> foldMap
        (\ (i, e) -> (if i /= 0 then Builder.word8 Literal.comma else mempty)
            <> f e)
        (Data.Array.assocs x)
    <> Builder.word8 Literal.rightSquareBracket

encodeObject :: (a -> Builder.Builder) -> Object.Object a -> Builder.Builder
encodeObject f (Object.Object x) =
    Builder.word8 Literal.leftCurlyBracket
    <> foldMap
        (\ (i, e) -> (if i /= 0 then Builder.word8 Literal.comma else mempty)
            <> encodePair encodeString f e)
        (Data.Array.assocs x)
    <> Builder.word8 Literal.rightCurlyBracket

encodePair :: (k -> Builder.Builder) -> (v -> Builder.Builder) -> Pair.Pair k v -> Builder.Builder
encodePair f g (Pair.Pair (x, y)) =
    f x
    <> Builder.word8 Literal.colon
    <> g y

encodeChar :: P.BoundedPrim Word.Word8
encodeChar =
    P.condB (== Literal.quotationMark) (encodeShortEscape Literal.quotationMark)
    . P.condB (== Literal.reverseSolidus) (encodeShortEscape Literal.reverseSolidus)
    . P.condB (== Literal.backspace) (encodeShortEscape Literal.latinSmallLetterB)
    . P.condB (== Literal.formFeed) (encodeShortEscape Literal.latinSmallLetterF)
    . P.condB (== Literal.newLine) (encodeShortEscape Literal.latinSmallLetterN)
    . P.condB (== Literal.carriageReturn) (encodeShortEscape Literal.latinSmallLetterR)
    . P.condB (== Literal.horizontalTabulation) (encodeShortEscape Literal.latinSmallLetterT)
    . P.condB (<= Literal.unitSeparator) encodeLongEscape
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
