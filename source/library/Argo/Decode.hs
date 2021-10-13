module Argo.Decode where

import Control.Applicative ((<|>))

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Array
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Word as Word

decodeValue :: Decoder.Decoder Value.Value
decodeValue =
    Value.Null <$> decodeNull
    <|> Value.Boolean <$> decodeBoolean
    <|> Value.Number <$> decodeNumber
    <|> Value.String <$> decodeString
    <|> Value.Array <$> decodeArray decodeValue
    <|> Value.Object <$> decodeObject decodeValue

decodeNull :: Decoder.Decoder Null.Null
decodeNull = Null.Null () <$ Decoder.byteString Literal.null <* Decoder.spaces

decodeBoolean :: Decoder.Decoder Boolean.Boolean
decodeBoolean = decodeFalse <|> decodeTrue

decodeNumber :: Decoder.Decoder Number.Number
decodeNumber = do
    ni <- fmap Maybe.isJust . Applicative.optional $ Decoder.word8 Literal.hyphenMinus
    i <- Decoder.takeWhile1 Decoder.isDigit
    Monad.when (ByteString.length i > 1 && ByteString.elemIndex Literal.digitZero i == Just 0)
        $ fail "leading zero"
    f <- fmap (Maybe.fromMaybe ByteString.empty) . Applicative.optional $ do
        Decoder.word8 Literal.fullStop
        Decoder.takeWhile1 Decoder.isDigit
    (ne, e) <- fmap (Maybe.fromMaybe (False, ByteString.empty)) . Applicative.optional $ do
        Monad.void
            . Decoder.satisfy
            $ \ x -> x == Literal.latinSmallLetterE || x == Literal.latinCapitalLetterE
        ne <- fmap (== Just Literal.hyphenMinus)
            . Applicative.optional
            . Decoder.satisfy
            $ \ x -> x == Literal.hyphenMinus || x == Literal.plusSign
        e <- Decoder.takeWhile1 Decoder.isDigit
        pure (ne, e)
    Decoder.spaces
    pure . Number.normalize $ Number.Number
        ((if ni then negate else id) $ (f7 i * 10 ^ ByteString.length f) + f7 f)
        ((if ne then negate else id) (f7 e) - intToInteger (ByteString.length f))

f7 :: ByteString.ByteString -> Integer
f7 = ByteString.foldl' (\ a e -> (a * 10) + word8ToInteger (e - 0x30)) 0

intToInteger :: Int -> Integer
intToInteger = fromIntegral

word8ToInteger :: Word.Word8 -> Integer
word8ToInteger = fromIntegral

decodeString :: Decoder.Decoder String.String
decodeString = do
    Decoder.word8 Literal.quotationMark
    b1 <- Decoder.get
    i <- case f3 b1 0 of
        Nothing -> fail "unterminated string"
        Just i -> pure i
    let (xs, b2) = ByteString.splitAt i b1
    Monad.when (ByteString.any (< Literal.space) xs) $ fail "unescaped control character"
    Decoder.put b2
    Decoder.word8 Literal.quotationMark
    Decoder.spaces
    case Text.decodeUtf8' xs of
        Left e -> fail $ show e
        Right x -> case f4 x of
            Nothing -> fail "invalid escape"
            Just y -> pure $ String.String y

-- finds the index at or after i of byte x
f1 :: Word.Word8 -> Int -> ByteString.ByteString -> Maybe Int
f1 x i = fmap (+ i) . ByteString.elemIndex x . ByteString.drop i

-- counts the number of x bytes ending at index i
f2 :: Word.Word8 -> Int -> ByteString.ByteString -> Int
f2 x i = ByteString.length . ByteString.takeWhileEnd (== x) . ByteString.take i

-- returns index of last character in string
f3 :: ByteString.ByteString -> Int -> Maybe Int
f3 b i = do
    j <- f1 Literal.quotationMark i b
    let n = f2 Literal.reverseSolidus j b
    if even n then Just j else f3 b $ j + 1

-- unescapes text
f4 :: Text.Text -> Maybe Text.Text
f4 = fmap (Text.pack . combineSurrogatePairs) . f5 . Text.unpack

combineSurrogatePairs :: String -> String
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

-- unescapes string
f5 :: String -> Maybe String
f5 xs = case xs of
    "" -> pure xs
    '\\' : x : ys -> case x of
        '"' -> ('"' :) <$> f5 ys
        '\\' -> ('\\' :) <$> f5 ys
        '/' -> ('/' :) <$> f5 ys
        'b' -> ('\b' :) <$> f5 ys
        'f' -> ('\f' :) <$> f5 ys
        'n' -> ('\n' :) <$> f5 ys
        'r' -> ('\r' :) <$> f5 ys
        't' -> ('\t' :) <$> f5 ys
        'u' -> let p = Char.isHexDigit in case ys of
            a : b : c : d : zs | p a && p b && p c && p d ->
                let
                    y = Char.chr
                        $ (0x1000 * Char.digitToInt a)
                        + (0x100 * Char.digitToInt b)
                        + (0x10 * Char.digitToInt c)
                        + Char.digitToInt d
                in (y :) <$> f5 zs
            _ -> fail "invalid long escape"
        _ -> fail "invalid short escape"
    x : ys -> (x :) <$> f5 ys

decodeArray :: Decoder.Decoder a -> Decoder.Decoder (Array.Array a)
decodeArray f = do
    Decoder.word8 Literal.leftSquareBracket
    Decoder.spaces
    xs <- decodeArrayWith f 0 []
    Decoder.word8 Literal.rightSquareBracket
    Decoder.spaces
    pure $ Array.Array xs

decodeObject :: Decoder.Decoder a -> Decoder.Decoder (Object.Object a)
decodeObject f = do
    Decoder.word8 Literal.leftCurlyBracket
    Decoder.spaces
    xs <- decodeArrayWith (decodePair decodeString f) 0 []
    Decoder.word8 Literal.rightCurlyBracket
    Decoder.spaces
    pure $ Object.Object xs

decodePair :: Decoder.Decoder k -> Decoder.Decoder v -> Decoder.Decoder (Pair.Pair k v)
decodePair f g = do
    k <- f
    Decoder.word8 Literal.colon
    Decoder.spaces
    v <- g
    pure $ Pair.Pair (k, v)

decodeArrayWith :: Decoder.Decoder a -> Int -> [(Int, a)] -> Decoder.Decoder (Data.Array.Array Int a)
decodeArrayWith f n xs = do
    m <- Applicative.optional $ do
        Monad.when (n /= 0) $ do
            Decoder.word8 Literal.comma
            Decoder.spaces
        f
    case m of
        Nothing -> pure $ Data.Array.array (0, n - 1) xs
        Just x -> decodeArrayWith f (n + 1) $ (n, x) : xs

decodeFalse :: Decoder.Decoder Boolean.Boolean
decodeFalse = Boolean.Boolean False <$ Decoder.byteString Literal.false <* Decoder.spaces

decodeTrue :: Decoder.Decoder Boolean.Boolean
decodeTrue = Boolean.Boolean True <$ Decoder.byteString Literal.true <* Decoder.spaces
