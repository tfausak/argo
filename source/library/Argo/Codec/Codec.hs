module Argo.Codec.Codec where

import Control.Applicative ((<|>))

import qualified Control.Applicative as Applicative

project :: (i -> f) -> Codec r w f o -> Codec r w i o
project f c = c { encode = encode c . f }

data Codec r w i o = Codec
    { decode :: r o
    , encode :: i -> w o
    }

instance (Functor r, Functor w) => Functor (Codec r w i) where
    fmap f c = Codec { decode = f <$> decode c, encode = fmap f . encode c }

instance (Applicative r, Applicative w) => Applicative (Codec r w i) where
    pure x = Codec { decode = pure x, encode = const $ pure x }
    cf <*> cx = Codec
        { decode = decode cf <*> decode cx
        , encode = \i -> encode cf i <*> encode cx i
        }

instance
    ( Applicative.Alternative r
    , Applicative.Alternative w
    ) => Applicative.Alternative (Codec r w i) where
    empty =
        Codec { decode = Applicative.empty, encode = const Applicative.empty }
    cx <|> cy = Codec
        { decode = decode cx <|> decode cy
        , encode = \i -> encode cx i <|> encode cy i
        }

map
    :: (Functor r, Functor w)
    => (a -> b)
    -> (b -> a)
    -> Codec r w a a
    -> Codec r w b b
map f g c =
    Codec { decode = f <$> decode c, encode = fmap f . encode c . g }

tap :: Functor f => (a -> f b) -> a -> f a
tap f x = x <$ f x

mapMaybe
    :: (Applicative.Alternative r, Applicative.Alternative w, Monad r, Monad w)
    => (o2 -> Maybe o1)
    -> (i1 -> Maybe i2)
    -> Codec r w i2 o2
    -> Codec r w i1 o1
mapMaybe f g c = Codec
    { decode = do
        o2 <- decode c
        toAlternative $ f o2
    , encode = \i1 -> do
        i2 <- toAlternative $ g i1
        o2 <- encode c i2
        toAlternative $ f o2
    }

toAlternative :: Applicative.Alternative m => Maybe a -> m a
toAlternative = maybe Applicative.empty pure
