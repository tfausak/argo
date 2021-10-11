module Argo.Decode where

import Control.Applicative ((<|>))

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

decodeValue :: Decode Value.Value
decodeValue =
    Value.Null <$> decodeNull
    <|> Value.Boolean <$> decodeBoolean
    <|> Value.Number <$> decodeNumber
    <|> Value.String <$> decodeString
    <|> Value.Array <$> decodeArray decodeValue
    <|> Value.Object <$> decodeObject decodeValue

decodeNull :: Decode Null.Null
decodeNull = Null.Null () <$ byteString Literal.null <* spaces

decodeBoolean :: Decode Boolean.Boolean
decodeBoolean = decodeFalse <|> decodeTrue

decodeNumber :: Decode Number.Number
decodeNumber = do
    ni <- fmap Maybe.isJust . Applicative.optional $ word8 Literal.hyphenMinus
    i <- takeWhile1 isDigit
    Monad.when (ByteString.length i > 1 && ByteString.elemIndex Literal.digitZero i == Just 0)
        $ fail "leading zero"
    f <- fmap (Maybe.fromMaybe ByteString.empty) . Applicative.optional $ do
        word8 Literal.fullStop
        takeWhile1 isDigit
    (ne, e) <- fmap (Maybe.fromMaybe (False, ByteString.empty)) . Applicative.optional $ do
        Monad.void
            . satisfy
            $ \ x -> x == Literal.latinSmallLetterE || x == Literal.latinCapitalLetterE
        ne <- fmap (== Just Literal.hyphenMinus)
            . Applicative.optional
            . satisfy
            $ \ x -> x == Literal.hyphenMinus || x == Literal.plusSign
        e <- takeWhile1 isDigit
        pure (ne, e)
    spaces
    pure . Number.normalize $ Number.Number
        ((if ni then negate else id) $ (f7 i * 10 ^ ByteString.length f) + f7 f)
        ((if ne then negate else id) (f7 e) - intToInteger (ByteString.length f))

f7 :: ByteString.ByteString -> Integer
f7 = ByteString.foldl' (\ a e -> (a * 10) + word8ToInteger (e - 0x30)) 0

intToInteger :: Int -> Integer
intToInteger = fromIntegral

word8ToInteger :: Word.Word8 -> Integer
word8ToInteger = fromIntegral

decodeString :: Decode String.String
decodeString = do
    word8 Literal.quotationMark
    b1 <- get
    i <- case f3 b1 0 of
        Nothing -> fail "unterminated string"
        Just i -> pure i
    let (xs, b2) = ByteString.splitAt i b1
    Monad.when (ByteString.any (< Literal.space) xs) $ fail "unescaped control character"
    put b2
    word8 Literal.quotationMark
    spaces
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

decodeArray :: Decode a -> Decode (Array.Array a)
decodeArray f = do
    word8 Literal.leftSquareBracket
    spaces
    xs <- decodeArrayWith f 0 []
    word8 Literal.rightSquareBracket
    spaces
    pure $ Array.Array xs

decodeObject :: Decode a -> Decode (Object.Object a)
decodeObject f = do
    word8 Literal.leftCurlyBracket
    spaces
    xs <- decodeArrayWith (decodePair decodeString f) 0 []
    word8 Literal.rightCurlyBracket
    spaces
    pure $ Object.Object xs

decodePair :: Decode k -> Decode v -> Decode (Pair.Pair k v)
decodePair f g = do
    k <- f
    word8 Literal.colon
    spaces
    v <- g
    pure $ Pair.Pair (k, v)

decodeArrayWith :: Decode a -> Int -> [(Int, a)] -> Decode (Data.Array.Array Int a)
decodeArrayWith f n xs = do
    m <- Applicative.optional $ do
        Monad.when (n /= 0) $ do
            word8 Literal.comma
            spaces
        f
    case m of
        Nothing -> pure $ Data.Array.array (0, n - 1) xs
        Just x -> decodeArrayWith f (n + 1) $ (n, x) : xs

decodeFalse :: Decode Boolean.Boolean
decodeFalse = Boolean.Boolean False <$ byteString Literal.false <* spaces

decodeTrue :: Decode Boolean.Boolean
decodeTrue = Boolean.Boolean True <$ byteString Literal.true <* spaces

newtype Decode a = Decode
    { run :: ByteString.ByteString -> Maybe (ByteString.ByteString, a)
    }

instance Functor Decode where
    fmap f d = Decode $ \ b1 -> case run d b1 of
        Nothing -> Nothing
        Just (b2, x) -> Just (b2, f x)

instance Applicative Decode where
    pure x = Decode $ \ b -> Just (b, x)
    df <*> dx = Decode $ \ b1 -> case run df b1 of
        Nothing -> Nothing
        Just (b2, f) -> case run dx b2 of
            Nothing -> Nothing
            Just (b3, x) -> Just (b3, f x)

instance Monad Decode where
    d >>= f = Decode $ \ b1 -> case run d b1 of
        Nothing -> Nothing
        Just (b2, x) -> run (f x) b2

instance MonadFail Decode where
    fail _ = Decode $ const Nothing

instance Applicative.Alternative Decode where
    empty = fail "empty"
    dx <|> dy = Decode $ \ b1 -> case run dx b1 of
        Nothing -> run dy b1
        Just (b2, x) -> Just (b2, x)

byteString :: ByteString.ByteString -> Decode ()
byteString x = do
    b1 <- get
    case ByteString.stripPrefix x b1 of
        Nothing -> fail $ "byteString: " <> show x
        Just b2 -> put b2

dropWhile :: (Word.Word8 -> Bool) -> Decode ()
dropWhile f = do
    b <- get
    put $ ByteString.dropWhile f b

eof :: Decode ()
eof = do
    b <- get
    Monad.unless (ByteString.null b) $ fail "eof"

get :: Decode ByteString.ByteString
get = Decode $ \ b -> Just (b, b)

isDigit :: Word.Word8 -> Bool
isDigit x = Literal.digitZero <= x && x <= Literal.digitNine

isSpace :: Word.Word8 -> Bool
isSpace x =
    x == Literal.space
    || x == Literal.horizontalTabulation
    || x == Literal.newLine
    || x == Literal.carriageReturn

put :: ByteString.ByteString -> Decode ()
put b = Decode $ \ _ -> Just (b, ())

satisfy :: (Word.Word8 -> Bool) -> Decode Word.Word8
satisfy f = do
    b1 <- get
    case ByteString.uncons b1 of
        Just (x, b2) | f x -> do
            put b2
            pure x
        _ -> fail "satisfy"

spaces :: Decode ()
spaces = Argo.Decode.dropWhile isSpace

takeWhile :: (Word.Word8 -> Bool) -> Decode ByteString.ByteString
takeWhile f = do
    b1 <- get
    let (x, b2) = ByteString.span f b1
    put b2
    pure x

takeWhile1 :: (Word.Word8 -> Bool) -> Decode ByteString.ByteString
takeWhile1 f = do
    x <- Argo.Decode.takeWhile f
    Monad.when (ByteString.null x) $ fail "takeWhile1"
    pure x

word8 :: Word.Word8 -> Decode ()
word8 = Monad.void . satisfy . (==)
