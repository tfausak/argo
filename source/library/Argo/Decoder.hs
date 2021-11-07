module Argo.Decoder where

import qualified Argo.Literal as Literal
import qualified Argo.Type.Result as Result
import qualified Argo.Vendor.ByteString as ByteString
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Word as Word

newtype Decoder a = Decoder
    { run :: ByteString.ByteString -> Result.Result (ByteString.ByteString, a)
    }

instance Functor Decoder where
    fmap f d = Decoder $ \ b1 -> case run d b1 of
        Result.Failure e -> Result.Failure e
        Result.Success (b2, x) -> Result.Success (b2, f x)

instance Applicative Decoder where
    pure x = Decoder $ \ b -> Result.Success (b, x)
    df <*> dx = Decoder $ \ b1 -> case run df b1 of
        Result.Failure e -> Result.Failure e
        Result.Success (b2, f) -> case run dx b2 of
            Result.Failure e -> Result.Failure e
            Result.Success (b3, x) -> Result.Success (b3, f x)

instance Monad Decoder where
    d >>= f = Decoder $ \ b1 -> case run d b1 of
        Result.Failure e -> Result.Failure e
        Result.Success (b2, x) -> run (f x) b2

instance MonadFail Decoder where
    fail = Decoder . const . Result.Failure

instance Applicative.Alternative Decoder where
    empty = fail "empty"
    dx <|> dy = Decoder $ \ b1 -> case run dx b1 of
        Result.Failure _ -> run dy b1
        Result.Success (b2, x) -> Result.Success (b2, x)

list :: Decoder a -> Decoder [a]
list f = listWith f []

listWith :: Decoder a -> [a] -> Decoder [a]
listWith f xs = do
    m <- Applicative.optional $ do
        Monad.unless (null xs) $ do
            word8 Literal.comma
            spaces
        f
    case m of
        Nothing -> pure $ reverse xs
        Just x -> listWith f $ x : xs

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
get = Decoder $ \ b -> Result.Success (b, b)

isDigit :: Word.Word8 -> Bool
isDigit x = Literal.digitZero <= x && x <= Literal.digitNine

isSpace :: Word.Word8 -> Bool
isSpace x =
    x == Literal.space
    || x == Literal.horizontalTabulation
    || x == Literal.newLine
    || x == Literal.carriageReturn

put :: ByteString.ByteString -> Decoder ()
put b = Decoder $ \ _ -> Result.Success (b, ())

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
