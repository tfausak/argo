{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.ToValue where

import qualified Argo.Encoder as Encoder
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Number as Number
import qualified Argo.Json.Value as Value
import qualified Argo.Pattern as Pattern
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Type.Config as Config
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.ByteString as ByteString
import qualified Argo.Vendor.Text as Text
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Word as Word
import qualified Numeric

class ToValue a where
    toValue :: a -> Value.Value

instance ToValue Value.Value where
    toValue = id

instance ToValue Bool where
    toValue = Pattern.Boolean

instance ToValue Char where
    toValue = toValue . Text.singleton

instance ToValue Int where
    toValue = toValue . toInteger

instance ToValue Int.Int8 where
    toValue = toValue . toInteger

instance ToValue Int.Int16 where
    toValue = toValue . toInteger

instance ToValue Int.Int32 where
    toValue = toValue . toInteger

instance ToValue Int.Int64 where
    toValue = toValue . toInteger

instance ToValue Word where
    toValue = toValue . toInteger

instance ToValue Word.Word8 where
    toValue = toValue . toInteger

instance ToValue Word.Word16 where
    toValue = toValue . toInteger

instance ToValue Word.Word32 where
    toValue = toValue . toInteger

instance ToValue Word.Word64 where
    toValue = toValue . toInteger

instance ToValue Integer where
    toValue = flip Pattern.Number 0

instance ToValue Float where
    toValue = realFloatToValue

instance ToValue Double where
    toValue = realFloatToValue

instance {-# OVERLAPPING #-} ToValue String where
    toValue = toValue . Text.pack

instance ToValue Text.Text where
    toValue = Pattern.String

instance ToValue Text.LazyText where
    toValue = toValue . Text.toStrict

instance ToValue a => ToValue (Maybe a) where
    toValue = maybe Pattern.Null toValue

instance ToValue () where
    toValue = const $ toValue ([] :: [Value.Value])

instance (ToValue a, ToValue b) => ToValue (a, b) where
    toValue (x, y) = toValue [toValue x, toValue y]

instance ToValue a => ToValue [a] where
    toValue = Pattern.Array . fmap toValue

instance ToValue a => ToValue (NonEmpty.NonEmpty a) where
    toValue = toValue . NonEmpty.toList

instance ToValue a => ToValue (Map.Map Text.Text a) where
    toValue x = Pattern.Object
        . fmap (\ (k, v) -> Member.Member (Pattern.Name k) (toValue v))
        $ Map.toAscList x

instance ToValue Pointer.Pointer where
    toValue = either (error . mappend "Pointer.toValue: " . show) toValue
        . Text.decodeUtf8'
        . ByteString.toStrict
        . Builder.toLazyByteString
        . snd
        . Encoder.run Config.initial
        . Pointer.encode

realFloatToValue :: RealFloat a => a -> Value.Value
realFloatToValue x
    | isNaN x = Pattern.Null
    | isInfinite x = Pattern.Null
    | otherwise =
        let isNegative = x < 0
        in Value.Number
        . (if isNegative then negateNumber else id)
        . uncurry digitsToNumber
        . Numeric.floatToDigits 10
        $ abs x

negateNumber :: Number.Number -> Number.Number
negateNumber (Number.Number x y) = Number.Number (-x) y

digitsToNumber :: [Int] -> Int -> Number.Number
digitsToNumber ds e = uncurry Number.number $ List.foldl'
    (\ (a, n) d -> (a * 10 + toInteger d, n - 1))
    (0, toInteger e)
    ds
