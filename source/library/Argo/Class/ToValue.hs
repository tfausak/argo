{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.ToValue where

import qualified Argo.Type as Type
import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Word as Word
import qualified Numeric

class ToValue a where
    toValue :: a -> Type.Value

instance ToValue Type.Value where
    toValue = id

instance ToValue Bool where
    toValue = Value.Boolean . Boolean.Boolean

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
    toValue = Value.Number . flip Number.number 0

instance ToValue Float where
    toValue = realFloatToValue

instance ToValue Double where
    toValue = realFloatToValue

instance {-# OVERLAPPING #-} ToValue String where
    toValue = toValue . Text.pack

instance ToValue Text.Text where
    toValue = Value.String . String.String

instance ToValue LazyText.Text where
    toValue = toValue . LazyText.toStrict

instance ToValue a => ToValue (Maybe a) where
    toValue = maybe (Value.Null $ Null.Null ()) toValue

instance ToValue () where
    toValue = const $ toValue ([] :: [Type.Value])

instance (ToValue a, ToValue b) => ToValue (a, b) where
    toValue (x, y) = toValue [toValue x, toValue y]

instance ToValue a => ToValue (Data.Array.Array Int a) where
    toValue = Value.Array . Array.Array . fmap toValue

instance ToValue a => ToValue [a] where
    toValue =
        let
            listToArray :: [b] -> Data.Array.Array Int b
            listToArray xs = Data.Array.listArray (0, length xs - 1) xs
        in toValue . listToArray

instance ToValue a => ToValue (NonEmpty.NonEmpty a) where
    toValue = toValue . NonEmpty.toList

realFloatToValue :: RealFloat a => a -> Value.Value
realFloatToValue x
    | isNaN x = Value.Null $ Null.Null ()
    | isInfinite x = Value.Null $ Null.Null ()
    | otherwise = Value.Number . uncurry digitsToNumber $ Numeric.floatToDigits 10 x

digitsToNumber :: [Int] -> Int -> Number.Number
digitsToNumber ds e = uncurry Number.number $ List.foldl'
    (\ (a, n) d -> (a * 10 + toInteger d, n - 1))
    (0, toInteger e)
    ds
