{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.FromValue where

import qualified Argo.Result as Result
import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Member as Member
import qualified Argo.Type.Name as Name
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Argo.Vendor.Text as Text
import qualified Data.Array
import qualified Data.Bits as Bits
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LazyText
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
        then fail $ "expected integer but got " <> show (Number.number x y)
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

instance FromValue LazyText.Text where
    fromValue = fmap LazyText.fromStrict . fromValue

instance FromValue a => FromValue (Maybe a) where
    fromValue x = case x of
        Value.Null _ -> pure Nothing
        _ -> Just <$> fromValue x

instance FromValue () where
    fromValue x = do
        [] <- fromValue x :: Result.Result [Value.Value]
        pure ()

instance (FromValue a, FromValue b) => FromValue (a, b) where
    fromValue x = do
        [y, z] <- fromValue x
        (,) <$> fromValue y <*> fromValue z

instance FromValue a => FromValue (Data.Array.Array Int a) where
    fromValue = withArray "Array" $ traverse fromValue

instance FromValue a => FromValue [a] where
    fromValue =
        let
            arrayToList :: Data.Array.Array Int b -> [b]
            arrayToList = Data.Array.elems
        in fmap arrayToList . fromValue

instance (FromValue a, Show a) => FromValue (NonEmpty.NonEmpty a) where
    fromValue value = do
        list <- fromValue value
        case NonEmpty.nonEmpty list of
            Nothing -> fail $ "expected non-empty list but got " <> show list
            Just nonEmpty -> pure nonEmpty

instance FromValue a => FromValue (Map.Map Text.Text a) where
    fromValue = withObject "Map"
        $ fmap Map.fromList
        . traverse (\ (Member.Member (Name.Name (String.String k)) v) -> (,) k <$> fromValue v)
        . Data.Array.elems

withBoolean :: String -> (Bool -> Result.Result a) -> Value.Value -> Result.Result a
withBoolean s f x = case x of
    Value.Boolean (Boolean.Boolean y) -> f y
    _ -> fail $ "expected " <> s <> " but got " <> show x

withNumber :: String -> (Integer -> Integer -> Result.Result a) -> Value.Value -> Result.Result a
withNumber s f x = case x of
    Value.Number (Number.Number y z) -> f y z
    _ -> fail $ "expected " <> s <> " but got " <> show x

withString :: String -> (Text.Text -> Result.Result a) -> Value.Value -> Result.Result a
withString s f x = case x of
    Value.String (String.String y) -> f y
    _ -> fail $ "expected " <> s <> " but got " <> show x

withArray :: String -> (Data.Array.Array Int Value.Value -> Result.Result a) -> Value.Value -> Result.Result a
withArray s f x = case x of
    Value.Array (Array.Array y) -> f y
    _ -> fail $ "expected " <> s <> " but got " <> show x

withObject :: String -> (Data.Array.Array Int (Member.Member Value.Value) -> Result.Result a) -> Value.Value -> Result.Result a
withObject s f x = case x of
    Value.Object (Object.Object y) -> f y
    _ -> fail $ "expected " <> s <> " but got " <> show x

viaInteger :: (Integral a, Bits.Bits a) => Value.Value -> Result.Result a
viaInteger value = do
    integer <- fromValue value
    case Bits.toIntegralSized (integer :: Integer) of
        Nothing -> fail $ "integer out of bounds " <> show integer
        Just x -> pure x
