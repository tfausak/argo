module Argo.Type.Decoder where

import qualified Argo.Literal as Literal
import qualified Argo.Type.Result as Result
import qualified Argo.Vendor.ByteString as ByteString
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity
import qualified Data.Word as Word

type Decoder = Trans.StateT ByteString.ByteString (Trans.ExceptT String Identity.Identity)

unwrap :: Decoder a -> ByteString.ByteString -> Either String (a, ByteString.ByteString)
unwrap d = Identity.runIdentity . Trans.runExceptT . Trans.runStateT d

run :: Decoder a -> ByteString.ByteString -> Result.Result a
run d = either Result.Failure Result.Success . fmap fst . unwrap (d <* eof)

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
    b1 <- Trans.get
    case ByteString.stripPrefix x b1 of
        Nothing -> Trans.lift . Trans.throwE $ "byteString: " <> show x
        Just b2 -> Trans.put b2

dropWhile :: (Word.Word8 -> Bool) -> Decoder ()
dropWhile f = do
    b <- Trans.get
    Trans.put $ ByteString.dropWhile f b

eof :: Decoder ()
eof = do
    b <- Trans.get
    Monad.unless (ByteString.null b) . Trans.lift $ Trans.throwE "eof"

isDigit :: Word.Word8 -> Bool
isDigit x = Literal.digitZero <= x && x <= Literal.digitNine

isSpace :: Word.Word8 -> Bool
isSpace x =
    x == Literal.space
    || x == Literal.horizontalTabulation
    || x == Literal.newLine
    || x == Literal.carriageReturn

satisfy :: (Word.Word8 -> Bool) -> Decoder Word.Word8
satisfy f = do
    b1 <- Trans.get
    case ByteString.uncons b1 of
        Just (x, b2) | f x -> do
            Trans.put b2
            pure x
        _ -> Trans.lift $ Trans.throwE "satisfy"

spaces :: Decoder ()
spaces = Argo.Type.Decoder.dropWhile isSpace

takeWhile :: (Word.Word8 -> Bool) -> Decoder ByteString.ByteString
takeWhile f = do
    b1 <- Trans.get
    let (x, b2) = ByteString.span f b1
    Trans.put b2
    pure x

takeWhile1 :: (Word.Word8 -> Bool) -> Decoder ByteString.ByteString
takeWhile1 f = do
    x <- Argo.Type.Decoder.takeWhile f
    Monad.when (ByteString.null x) . Trans.lift $ Trans.throwE "takeWhile1"
    pure x

word8 :: Word.Word8 -> Decoder ()
word8 = Monad.void . satisfy . (==)
