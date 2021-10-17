{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.FromValue where

import Control.Monad ((<=<))

import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array
import qualified Data.Bits as Bits
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Word as Word

class FromValue a where
    fromValue :: Value.Value -> Maybe a

instance FromValue Value.Value where
    fromValue = Just

instance FromValue Bool where
    fromValue = withBoolean "Bool" pure

instance FromValue Char where
    fromValue = withString "Char" $ \ x -> case Text.uncons x of
        Just (y, z) | Text.null z -> pure y
        _ -> fail "not singleton"

instance FromValue Int where
    fromValue =
        let
            integerToInt :: Integer -> Maybe Int
            integerToInt = Bits.toIntegralSized
        in integerToInt <=< fromValue

instance FromValue Int.Int8 where
    fromValue =
        let
            integerToInt8 :: Integer -> Maybe Int.Int8
            integerToInt8 = Bits.toIntegralSized
        in integerToInt8 <=< fromValue

instance FromValue Int.Int16 where
    fromValue =
        let
            integerToInt16 :: Integer -> Maybe Int.Int16
            integerToInt16 = Bits.toIntegralSized
        in integerToInt16 <=< fromValue

instance FromValue Int.Int32 where
    fromValue =
        let
            integerToInt32 :: Integer -> Maybe Int.Int32
            integerToInt32 = Bits.toIntegralSized
        in integerToInt32 <=< fromValue

instance FromValue Int.Int64 where
    fromValue =
        let
            integerToInt64 :: Integer -> Maybe Int.Int64
            integerToInt64 = Bits.toIntegralSized
        in integerToInt64 <=< fromValue

instance FromValue Word where
    fromValue =
        let
            integerToWord :: Integer -> Maybe Word
            integerToWord = Bits.toIntegralSized
        in integerToWord <=< fromValue

instance FromValue Word.Word8 where
    fromValue =
        let
            integerToWord8 :: Integer -> Maybe Word.Word8
            integerToWord8 = Bits.toIntegralSized
        in integerToWord8 <=< fromValue

instance FromValue Word.Word16 where
    fromValue =
        let
            integerToWord16 :: Integer -> Maybe Word.Word16
            integerToWord16 = Bits.toIntegralSized
        in integerToWord16 <=< fromValue

instance FromValue Word.Word32 where
    fromValue =
        let
            integerToWord32 :: Integer -> Maybe Word.Word32
            integerToWord32 = Bits.toIntegralSized
        in integerToWord32 <=< fromValue

instance FromValue Word.Word64 where
    fromValue =
        let
            integerToWord64 :: Integer -> Maybe Word.Word64
            integerToWord64 = Bits.toIntegralSized
        in integerToWord64 <=< fromValue

instance FromValue Integer where
    fromValue = withNumber "Integer" $ \ x y ->
        if y < 0 then fail "fractional" else pure $ x * 10 ^ y

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

instance FromValue a => FromValue (Data.Array.Array Int a) where
    fromValue = withArray "Array" $ traverse fromValue

instance FromValue a => FromValue [a] where
    fromValue =
        let
            arrayToList :: Data.Array.Array Int b -> [b]
            arrayToList = Data.Array.elems
        in fmap arrayToList . fromValue

instance FromValue a => FromValue (NonEmpty.NonEmpty a) where
    fromValue = NonEmpty.nonEmpty <=< fromValue

withBoolean :: String -> (Bool -> Maybe a) -> Value.Value -> Maybe a
withBoolean s f x = case x of
    Value.Boolean (Boolean.Boolean y) -> f y
    _ -> fail s

withNumber :: String -> (Integer -> Integer -> Maybe a) -> Value.Value -> Maybe a
withNumber s f x = case x of
    Value.Number (Number.Number y z) -> f y z
    _ -> fail s

withString :: String -> (Text.Text -> Maybe a) -> Value.Value -> Maybe a
withString s f x = case x of
    Value.String (String.String y) -> f y
    _ -> fail s

withArray :: String -> (Data.Array.Array Int Value.Value -> Maybe a) -> Value.Value -> Maybe a
withArray s f x = case x of
    Value.Array (Array.Array y) -> f y
    _ -> fail s

withObject :: String -> (Data.Array.Array Int (Pair.Pair String.String Value.Value) -> Maybe a) -> Value.Value -> Maybe a
withObject s f x = case x of
    Value.Object (Object.Object y) -> f y
    _ -> fail s
