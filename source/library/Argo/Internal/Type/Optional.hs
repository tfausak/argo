module Argo.Internal.Type.Optional where

newtype Optional a
    = Optional (Maybe a)
    deriving (Eq, Show)

fromMaybe :: Maybe a -> Optional a
fromMaybe = Optional

toMaybe :: Optional a -> Maybe a
toMaybe (Optional x) = x

nothing :: Optional a
nothing = fromMaybe Nothing

just :: a -> Optional a
just = fromMaybe . Just
