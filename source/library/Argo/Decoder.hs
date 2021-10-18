module Argo.Decoder where

import qualified Argo.Literal as Literal
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Array as Array
import qualified Data.ByteString as ByteString
import qualified Data.Word as Word

newtype Decoder a = Decoder
    { run :: ByteString.ByteString -> Either String (ByteString.ByteString, a)
    }

instance Functor Decoder where
    fmap f d = Decoder $ \ b1 -> case run d b1 of
        Left e -> Left e
        Right (b2, x) -> Right (b2, f x)

instance Applicative Decoder where
    pure x = Decoder $ \ b -> Right (b, x)
    df <*> dx = Decoder $ \ b1 -> case run df b1 of
        Left e -> Left e
        Right (b2, f) -> case run dx b2 of
            Left e -> Left e
            Right (b3, x) -> Right (b3, f x)

instance Monad Decoder where
    d >>= f = Decoder $ \ b1 -> case run d b1 of
        Left e -> Left e
        Right (b2, x) -> run (f x) b2

instance MonadFail Decoder where
    fail = Decoder . const . Left

instance Applicative.Alternative Decoder where
    empty = fail "empty"
    dx <|> dy = Decoder $ \ b1 -> case run dx b1 of
        Left _ -> run dy b1
        Right (b2, x) -> Right (b2, x)

array :: Decoder a -> Decoder (Array.Array Int a)
array f = arrayWith f 0 []

arrayWith :: Decoder a -> Int -> [(Int, a)] -> Decoder (Array.Array Int a)
arrayWith f n xs = do
    m <- Applicative.optional $ do
        Monad.when (n /= 0) $ do
            word8 Literal.comma
            spaces
        f
    case m of
        Nothing -> pure $ Array.array (0, n - 1) xs
        Just x -> arrayWith f (n + 1) $ (n, x) : xs

byteString :: ByteString.ByteString -> Decoder ()
byteString x = do
    b1 <- get
    case ByteString.stripPrefix x b1 of
        Nothing -> fail $ "byteString: " <> show x
        Just b2 -> put b2

dropWhile :: (Word.Word8 -> Bool) -> Decoder ()
dropWhile f = do
    b <- get
    put $ ByteString.dropWhile f b

eof :: Decoder ()
eof = do
    b <- get
    Monad.unless (ByteString.null b) $ fail "eof"

get :: Decoder ByteString.ByteString
get = Decoder $ \ b -> Right (b, b)

isDigit :: Word.Word8 -> Bool
isDigit x = Literal.digitZero <= x && x <= Literal.digitNine

isSpace :: Word.Word8 -> Bool
isSpace x =
    x == Literal.space
    || x == Literal.horizontalTabulation
    || x == Literal.newLine
    || x == Literal.carriageReturn

put :: ByteString.ByteString -> Decoder ()
put b = Decoder $ \ _ -> Right (b, ())

satisfy :: (Word.Word8 -> Bool) -> Decoder Word.Word8
satisfy f = do
    b1 <- get
    case ByteString.uncons b1 of
        Just (x, b2) | f x -> do
            put b2
            pure x
        _ -> fail "satisfy"

spaces :: Decoder ()
spaces = Argo.Decoder.dropWhile isSpace

takeWhile :: (Word.Word8 -> Bool) -> Decoder ByteString.ByteString
takeWhile f = do
    b1 <- get
    let (x, b2) = ByteString.span f b1
    put b2
    pure x

takeWhile1 :: (Word.Word8 -> Bool) -> Decoder ByteString.ByteString
takeWhile1 f = do
    x <- Argo.Decoder.takeWhile f
    Monad.when (ByteString.null x) $ fail "takeWhile1"
    pure x

word8 :: Word.Word8 -> Decoder ()
word8 = Monad.void . satisfy . (==)
