{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.ToValue where

import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Number as Number
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array
import qualified Data.Text as Text

class ToValue a where
    toValue :: a -> Value.Value

instance ToValue Value.Value where
    toValue = id

instance ToValue Bool where
    toValue = Value.Boolean . Boolean.Boolean

instance ToValue Char where
    toValue = toValue . Text.singleton

instance ToValue Integer where
    toValue = Value.Number . Number.normalize . flip Number.Number 0

instance ToValue Text.Text where
    toValue = Value.String . String.String

instance ToValue a => ToValue (Data.Array.Array Int a) where
    toValue = Value.Array . Array.Array . fmap toValue

instance ToValue a => ToValue [a] where
    toValue =
        let
            listToArray :: [b] -> Data.Array.Array Int b
            listToArray xs = Data.Array.listArray (0, length xs - 1) xs
        in toValue . listToArray
