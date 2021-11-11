{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.FromValue where

import qualified Argo.Json.Member as Member
import qualified Argo.Json.Value as Value
import qualified Argo.Pattern as Pattern
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Type.Decimal as Decimal
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Vendor.Text as Text
import qualified Data.Bits as Bits
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Word as Word

class FromValue a where
    fromValue :: Value.Value -> Either String a

instance FromValue Value.Value where
    fromValue = Right

instance FromValue Bool where
    fromValue = withBoolean "Bool" pure

instance FromValue Char where
    fromValue = withString "Char" $ \x -> case Text.uncons x of
        Just (y, z) | Text.null z -> pure y
        _ -> Left $ "expected single character but got " <> show x

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
    fromValue = withNumber "Integer" $ \x@(Decimal.Decimal s e) -> if e < 0
        then Left $ "expected integer but got " <> show x
        else pure $ s * 10 ^ e

instance FromValue Float where
    fromValue = withNumber "Float" $ pure . fromRational . Decimal.toRational

instance FromValue Double where
    fromValue = withNumber "Double" $ pure . fromRational . Decimal.toRational

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
    fromValue = withArray "()" $ \xs -> case xs of
        [] -> pure ()
        _ -> Left $ "expected empty list but got " <> show xs

instance (FromValue a, FromValue b) => FromValue (a, b) where
    fromValue = withArray "(a, b)" $ \xs -> case xs of
        [x, y] -> (,) <$> fromValue x <*> fromValue y
        _ -> Left $ "expected tuple but got " <> show xs

instance FromValue a => FromValue [a] where
    fromValue = withArray "[a]" $ traverse fromValue

instance FromValue a => FromValue (NonEmpty.NonEmpty a) where
    fromValue value = do
        list <- fromValue value
        case NonEmpty.nonEmpty list of
            Nothing -> Left "unexpected empty list"
            Just nonEmpty -> pure nonEmpty

instance FromValue a => FromValue (Map.Map Text.Text a) where
    fromValue = withObject "Map" $ fmap Map.fromList . traverse
        (\(Member.Member (Pattern.Name k) v) -> (,) k <$> fromValue v)

instance FromValue Pointer.Pointer where
    fromValue =
        withString "Pointer" $ Decoder.run Pointer.decode . Text.encodeUtf8

withBoolean
    :: String -> (Bool -> Either String a) -> Value.Value -> Either String a
withBoolean s f x = case x of
    Pattern.Boolean y -> f y
    _ -> Left $ "expected " <> s <> " but got " <> show x

withNumber
    :: String
    -> (Decimal.Decimal -> Either String a)
    -> Value.Value
    -> Either String a
withNumber s f x = case x of
    Pattern.Number y -> f y
    _ -> Left $ "expected " <> s <> " but got " <> show x

withString
    :: String
    -> (Text.Text -> Either String a)
    -> Value.Value
    -> Either String a
withString s f x = case x of
    Pattern.String y -> f y
    _ -> Left $ "expected " <> s <> " but got " <> show x

withArray
    :: String
    -> ([Value.Value] -> Either String a)
    -> Value.Value
    -> Either String a
withArray s f x = case x of
    Pattern.Array y -> f y
    _ -> Left $ "expected " <> s <> " but got " <> show x

withObject
    :: String
    -> ([Member.MemberOf Value.Value] -> Either String a)
    -> Value.Value
    -> Either String a
withObject s f x = case x of
    Pattern.Object y -> f y
    _ -> Left $ "expected " <> s <> " but got " <> show x

viaInteger :: (Integral a, Bits.Bits a) => Value.Value -> Either String a
viaInteger value = do
    integer <- fromValue value
    case Bits.toIntegralSized (integer :: Integer) of
        Nothing -> Left $ "integer out of bounds " <> show integer
        Just x -> pure x
