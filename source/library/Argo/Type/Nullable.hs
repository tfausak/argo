module Argo.Type.Nullable where

newtype Nullable a
    = Nullable (Maybe a)
    deriving (Eq, Show)

fromMaybe :: Maybe a -> Nullable a
fromMaybe = Nullable

toMaybe :: Nullable a -> Maybe a
toMaybe (Nullable x) = x

nothing :: Nullable a
nothing = fromMaybe Nothing

just :: a -> Nullable a
just = fromMaybe . Just
