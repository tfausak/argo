module Argo.Encode
    ( encode
    ) where

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
import qualified Data.List as List

encode :: Value.Value -> Builder.Builder
encode x = case x of
    Value.Null y -> encodeNull y
    Value.Boolean y -> encodeBoolean y
    Value.Number y -> encodeNumber y
    Value.String y -> encodeString y
    Value.Array y -> encodeArray encode y
    Value.Object y -> encodeObject encode y

encodeNull :: Null.Null -> Builder.Builder
encodeNull _ =
    Builder.string7 "null"

encodeBoolean :: Boolean.Boolean -> Builder.Builder
encodeBoolean (Boolean.Boolean x) =
    Builder.string7 $ if x then "true" else "false"

encodeNumber :: Number.Number -> Builder.Builder
encodeNumber (Number.Number x y) =
    if y == 0
    then Builder.integerDec x
    else Builder.integerDec x <> Builder.char7 'e' <> Builder.integerDec y

encodeString :: String.String -> Builder.Builder
encodeString (String.String x) =
    Builder.char7 '"'
    <> Text.encodeUtf8BuilderEscaped encodeChar x
    <> Builder.char7 '"'

encodeArray :: (a -> Builder.Builder) -> Array.Array a -> Builder.Builder
encodeArray f (Array.Array x) =
    Builder.char7 '['
    <> (mconcat
        . List.intersperse (Builder.char7 ',')
        . fmap f
        $ Data.Array.elems x)
    <> Builder.char7 ']'

encodeObject :: (a -> Builder.Builder) -> Object.Object a -> Builder.Builder
encodeObject f (Object.Object x) =
    Builder.char7 '{'
    <> (mconcat
        . List.intersperse (Builder.char7 ',')
        . fmap (encodePair encodeString f)
        $ Data.Array.elems x)
    <> Builder.char7 '}'

encodePair :: (k -> Builder.Builder) -> (v -> Builder.Builder) -> Pair.Pair k v -> Builder.Builder
encodePair f g (Pair.Pair (x, y)) =
    f x
    <> Builder.char7 ':'
    <> g y

encodeChar :: P.BoundedPrim Word.Word8
encodeChar =
    P.condB (== 0x22) (encodeShortEscape 0x22) -- U+0022 quotation mark
    . P.condB (== 0x5c) (encodeShortEscape 0x5c) -- U+005c reverse solidus
    . P.condB (== 0x08) (encodeShortEscape 0x62) -- U+0008 backspace
    . P.condB (== 0x0c) (encodeShortEscape 0x66) -- U+000c form feed
    . P.condB (== 0x0a) (encodeShortEscape 0x6e) -- U+000a new line
    . P.condB (== 0x0d) (encodeShortEscape 0x72) -- U+000d carriage return
    . P.condB (== 0x09) (encodeShortEscape 0x74) -- U+0009 horizontal tabulation
    . P.condB (<= 0x1f) encodeLongEscape
    $ P.liftFixedToBounded P.word8

encodeShortEscape :: Word.Word8 -> P.BoundedPrim a
encodeShortEscape x = P.liftFixedToBounded $ const (0x5c, x)
    P.>$< P.word8
    P.>*< P.word8

encodeLongEscape :: P.BoundedPrim Word.Word8
encodeLongEscape = P.liftFixedToBounded $ (\ x -> (0x5c, (0x75, word8ToWord16 x)))
    P.>$< P.word8
    P.>*< P.word8
    P.>*< P.word16HexFixed

word8ToWord16 :: Word.Word8 -> Word.Word16
word8ToWord16 = fromIntegral
