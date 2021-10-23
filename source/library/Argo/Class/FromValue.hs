{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.FromValue where

import qualified Argo.Json.Number as Number
import qualified Argo.Json.Value as Value
import qualified Argo.Pattern as Pattern
import qualified Argo.Result as Result
import qualified Argo.Vendor.Text as Text
import qualified Data.Bits as Bits
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Word as Word

class FromValue a where
    fromValue :: Value.Value -> Result.Result a

instance FromValue Value.Value where
    fromValue = Result.Success

instance FromValue Bool where
    fromValue = withBoolean "Bool" pure

instance FromValue Char where
    fromValue = withString "Char" $ \ x -> case Text.uncons x of
        Just (y, z) | Text.null z -> pure y
        _ -> fail $ "expected single character but got " <> show x

instance FromValue Int where
    fromValue = viaInteger

instance FromValue Int.Int8 where
    fromValue = viaInteger

instance FromValue Int.Int16 where
    fromValue = viaInteger

instance FromValue Int.Int32 where
    fromValue = viaInteger

instance FromValue Int.Int64 where
    fromValue = viaInteger

instance FromValue Word where
    fromValue = viaInteger

instance FromValue Word.Word8 where
    fromValue = viaInteger

instance FromValue Word.Word16 where
    fromValue = viaInteger

instance FromValue Word.Word32 where
    fromValue = viaInteger

instance FromValue Word.Word64 where
    fromValue = viaInteger

instance FromValue Integer where
    fromValue = withNumber "Integer" $ \ x y ->
        if y < 0
        then fail $ "expected integer but got " <> show (Pattern.Number x y)
        else pure $ x * 10 ^ y

instance FromValue Float where
    fromValue = withNumber "Float" $ \ x y ->
        pure . fromRational . Number.toRational $ Number.Number x y

instance FromValue Double where
    fromValue = withNumber "Double" $ \ x y ->
        pure . fromRational . Number.toRational $ Number.Number x y

instance {-# OVERLAPPING #-} FromValue String where
    fromValue = fmap Text.unpack . fromValue

instance FromValue Text.Text where
    fromValue = withString "Text" pure

instance FromValue Text.LazyText where
    fromValue = fmap Text.fromStrict . fromValue

instance FromValue a => FromValue (Maybe a) where
    fromValue x = case x of
        Pattern.Null -> pure Nothing
        _ -> Just <$> fromValue x

instance FromValue () where
    fromValue = withArray "()" $ \ xs -> case xs of
        [] -> pure ()
        _ -> fail $ "expected empty list but got " <> show xs

instance (FromValue a, FromValue b) => FromValue (a, b) where
    fromValue = withArray "(a, b)" $ \ xs -> case xs of
        [x, y] -> (,) <$> fromValue x <*> fromValue y
        _ -> fail $ "expected tuple but got " <> show xs

instance FromValue a => FromValue [a] where
    fromValue = withArray "[a]" $ traverse fromValue

instance FromValue a => FromValue (NonEmpty.NonEmpty a) where
    fromValue value = do
        list <- fromValue value
        case NonEmpty.nonEmpty list of
            Nothing -> fail "unexpected empty list"
            Just nonEmpty -> pure nonEmpty

instance FromValue a => FromValue (Map.Map Text.Text a) where
    fromValue = withObject "Map"
        $ fmap Map.fromList
        . traverse (\ (Pattern.Member (Pattern.Name k) v) -> (,) k <$> fromValue v)

withBoolean :: String -> (Bool -> Result.Result a) -> Value.Value -> Result.Result a
withBoolean s f x = case x of
    Pattern.Boolean y -> f y
    _ -> fail $ "expected " <> s <> " but got " <> show x

withNumber :: String -> (Integer -> Integer -> Result.Result a) -> Value.Value -> Result.Result a
withNumber s f x = case x of
    Pattern.Number y z -> f y z
    _ -> fail $ "expected " <> s <> " but got " <> show x

withString :: String -> (Text.Text -> Result.Result a) -> Value.Value -> Result.Result a
withString s f x = case x of
    Pattern.String y -> f y
    _ -> fail $ "expected " <> s <> " but got " <> show x

withArray :: String -> (Pattern.Array -> Result.Result a) -> Value.Value -> Result.Result a
withArray s f x = case x of
    Pattern.Array y -> f y
    _ -> fail $ "expected " <> s <> " but got " <> show x

withObject :: String -> (Pattern.Object -> Result.Result a) -> Value.Value -> Result.Result a
withObject s f x = case x of
    Pattern.Object y -> f y
    _ -> fail $ "expected " <> s <> " but got " <> show x

viaInteger :: (Integral a, Bits.Bits a) => Value.Value -> Result.Result a
viaInteger value = do
    integer <- fromValue value
    case Bits.toIntegralSized (integer :: Integer) of
        Nothing -> fail $ "integer out of bounds " <> show integer
        Just x -> pure x
