module Argo.Codec.Codec where

import Control.Applicative ((<|>))

import qualified Control.Applicative as Applicative

data Codec r w s i o = Codec
    { decode :: r o
    , encode :: i -> w o
    , schema :: s
    }

instance (Functor r, Functor w) => Functor (Codec r w s i) where
    fmap f c = Codec
        { decode = f <$> decode c
        , encode = fmap f . encode c
        , schema = schema c
        }

instance
    ( Applicative r
    , Applicative w
    , Monoid s
    ) => Applicative (Codec r w s i) where
    pure x =
        Codec { decode = pure x, encode = const $ pure x, schema = mempty }
    cf <*> cx = Codec
        { decode = decode cf <*> decode cx
        , encode = \i -> encode cf i <*> encode cx i
        , schema = schema cf <> schema cx
        }

instance
    ( Applicative.Alternative r
    , Applicative.Alternative w
    , Monoid s
    ) => Applicative.Alternative (Codec r w s i) where
    empty = Codec
        { decode = Applicative.empty
        , encode = const Applicative.empty
        , schema = mempty
        }
    cx <|> cy = Codec
        { decode = decode cx <|> decode cy
        , encode = \i -> encode cx i <|> encode cy i
        , schema = schema cx <> schema cy
        }

map
    :: (Functor r, Functor w)
    => (a -> b)
    -> (b -> a)
    -> Codec r w s a a
    -> Codec r w s b b
map f g c = Codec
    { decode = f <$> decode c
    , encode = fmap f . encode c . g
    , schema = schema c
    }

mapMaybe
    :: (Applicative.Alternative r, Applicative.Alternative w, Monad r, Monad w)
    => (o2 -> Maybe o1)
    -> (i1 -> Maybe i2)
    -> Codec r w s i2 o2
    -> Codec r w s i1 o1
mapMaybe f g c = Codec
    { decode = do
        o2 <- decode c
        toAlternative $ f o2
    , encode = \i1 -> do
        i2 <- toAlternative $ g i1
        o2 <- encode c i2
        toAlternative $ f o2
    , schema = schema c
    }

project :: (i -> f) -> Codec r w s f o -> Codec r w s i o
project f c = c { encode = encode c . f }

tap :: Functor f => (a -> f b) -> a -> f a
tap f x = x <$ f x

toAlternative :: Applicative.Alternative m => Maybe a -> m a
toAlternative = maybe Applicative.empty pure
