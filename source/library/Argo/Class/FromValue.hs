{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.FromValue where

import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText

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

instance FromValue Integer where
    fromValue = withNumber "Integer" $ \ x y ->
        if y < 0 then fail "fractional" else pure $ x * 10 ^ y

instance FromValue Text.Text where
    fromValue = withString "Text" pure

instance FromValue LazyText.Text where
    fromValue = fmap LazyText.fromStrict . fromValue

instance FromValue a => FromValue (Data.Array.Array Int a) where
    fromValue = withArray "Array" $ traverse fromValue

instance FromValue a => FromValue [a] where
    fromValue =
        let
            arrayToList :: Data.Array.Array Int b -> [b]
            arrayToList = Data.Array.elems
        in fmap arrayToList . fromValue

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
