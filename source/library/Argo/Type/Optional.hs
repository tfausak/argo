module Argo.Type.Optional where

newtype Optional a
    = Optional (Maybe a)
    deriving (Eq, Show)

fromMaybe :: Maybe a -> Optional a
fromMaybe = Optional

toMaybe :: Optional a -> Maybe a
toMaybe (Optional x) = x
