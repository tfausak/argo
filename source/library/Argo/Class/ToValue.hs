{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.ToValue where

import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Member as Member
import qualified Argo.Type.Name as Name
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Word as Word
import qualified Numeric

class ToValue a where
    toValue :: a -> Value.Value

instance ToValue Value.Value where
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
    toValue = const $ toValue ([] :: [Value.Value])

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

instance ToValue a => ToValue (Map.Map Text.Text a) where
    toValue x = Value.Object
        . Object.Object
        . Data.Array.listArray (0, Map.size x - 1)
        . fmap (\ (k, v) -> Member.Member (Name.Name (String.String k)) (toValue v))
        $ Map.toAscList x

realFloatToValue :: RealFloat a => a -> Value.Value
realFloatToValue x
    | isNaN x = Value.Null $ Null.Null ()
    | isInfinite x = Value.Null $ Null.Null ()
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
