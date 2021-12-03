module Argo.Type.Codec where

import Control.Applicative ((<|>))

import qualified Argo.Json.Array as Array
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Null as Null
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Text as Text

decodeWith :: ValueCodec a -> Value.Value -> Either String a
decodeWith c =
    Identity.runIdentity . Trans.runExceptT . Trans.runReaderT (decode c)

encodeWith :: ValueCodec a -> a -> Value.Value
encodeWith c x =
    snd
        . Identity.runIdentity
        . Trans.runStateT (Trans.runMaybeT $ encode c x)
        . Value.Null
        $ Null.fromUnit ()

project :: (i -> f) -> CodecOf r w f o -> CodecOf r w i o
project f c = c { encode = encode c . f }

data CodecOf r w i o = Codec
    { decode :: r o
    , encode :: i -> w o
    }

instance (Functor r, Functor w) => Functor (CodecOf r w i) where
    fmap f c = Codec { decode = f <$> decode c, encode = fmap f . encode c }

instance (Applicative r, Applicative w) => Applicative (CodecOf r w i) where
    pure x = Codec { decode = pure x, encode = const $ pure x }
    cf <*> cx = Codec
        { decode = decode cf <*> decode cx
        , encode = \i -> encode cf i <*> encode cx i
        }

instance
    ( Applicative.Alternative r
    , Applicative.Alternative w
    ) => Applicative.Alternative (CodecOf r w i) where
    empty =
        Codec { decode = Applicative.empty, encode = const Applicative.empty }
    cx <|> cy = Codec
        { decode = decode cx <|> decode cy
        , encode = \i -> encode cx i <|> encode cy i
        }

type Codec r w a = CodecOf r w a a

dimap
    :: (Functor r, Functor w)
    => (a -> b)
    -> (b -> a)
    -> Codec r w a
    -> Codec r w b
dimap f g c =
    Codec { decode = f <$> decode c, encode = fmap f . encode c . g }

tap :: Functor f => (a -> f b) -> a -> f a
tap f x = x <$ f x

type ValueCodec a
    = Codec
          (Trans.ReaderT Value.Value (Trans.ExceptT String Identity.Identity))
          (Trans.MaybeT (Trans.StateT Value.Value Identity.Identity))
          a

arrayCodec :: ValueCodec (Array.Array Value.Value)
arrayCodec = Codec
    { decode = do
        x <- Trans.ask
        case x of
            Value.Array y -> pure y
            _ ->
                Trans.lift . Trans.throwE $ "expected Array but got " <> show x
    , encode = \x -> do
        Trans.lift . Trans.put $ Value.Array x
        pure x
    }

objectCodec :: ValueCodec (Object.Object Value.Value)
objectCodec = Codec
    { decode = do
        x <- Trans.ask
        case x of
            Value.Object y -> pure y
            _ ->
                Trans.lift
                    . Trans.throwE
                    $ "expected Object but got "
                    <> show x
    , encode = \x -> do
        Trans.lift . Trans.put $ Value.Object x
        pure x
    }

mapMaybe
    :: (Applicative.Alternative r, Applicative.Alternative w, Monad r, Monad w)
    => (o2 -> Maybe o1)
    -> (i1 -> Maybe i2)
    -> CodecOf r w i2 o2
    -> CodecOf r w i1 o1
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

tagged :: String -> ValueCodec a -> ValueCodec a
tagged t c =
    dimap snd ((,) ())
        . fromObjectCodec Permission.Allow
        $ (,)
        <$> project
                fst
                (required
                    (Name.fromString . String.fromText $ Text.pack "type")
                    (literalCodec
                        (Value.String . String.fromText $ Text.pack t)
                    )
                )
        <*> project
                snd
                (required
                    (Name.fromString . String.fromText $ Text.pack "value")
                    c
                )

literalCodec :: Value.Value -> ValueCodec ()
literalCodec expected = Codec
    { decode = do
        actual <- Trans.ask
        Monad.when (actual /= expected)
            . Trans.lift
            . Trans.throwE
            $ "expected "
            <> show expected
            <> " but got "
            <> show actual
    , encode = const . Trans.lift $ Trans.put expected
    }

type ListCodec e a
    = Codec
          (Trans.StateT [e] (Trans.ExceptT String Identity.Identity))
          (Trans.WriterT [e] Identity.Identity)
          a

fromListCodec
    :: ValueCodec [e] -> Permission.Permission -> ListCodec e a -> ValueCodec a
fromListCodec ce p ca = Codec
    { decode = do
        xs <- decode ce
        case
                Identity.runIdentity . Trans.runExceptT $ Trans.runStateT
                    (decode ca)
                    xs
            of
                Left x -> Trans.lift $ Trans.throwE x
                Right (x, ys) -> do
                    case (p, ys) of
                        (Permission.Forbid, _ : _) ->
                            Trans.lift $ Trans.throwE "leftover elements"
                        _ -> pure ()
                    pure x
    , encode = \x -> do
        Monad.void
            . encode ce
            . snd
            . Identity.runIdentity
            . Trans.runWriterT
            $ encode ca x
        pure x
    }

type ArrayCodec a = ListCodec Value.Value a

fromArrayCodec :: Permission.Permission -> ArrayCodec a -> ValueCodec a
fromArrayCodec = fromListCodec $ dimap Array.toList Array.fromList arrayCodec

element :: ValueCodec a -> ArrayCodec a
element c = Codec
    { decode = do
        l <- Trans.get
        case l of
            [] -> Trans.lift $ Trans.throwE "unexpected empty list"
            h : t -> case decodeWith c h of
                Left y -> Trans.lift $ Trans.throwE y
                Right y -> do
                    Trans.put t
                    pure y
    , encode = \x -> do
        Trans.tell [encodeWith c x]
        pure x
    }

type ObjectCodec a = ListCodec (Member.Member Value.Value) a

fromObjectCodec :: Permission.Permission -> ObjectCodec a -> ValueCodec a
fromObjectCodec =
    fromListCodec $ dimap Object.toList Object.fromList objectCodec

required :: Name.Name -> ValueCodec a -> ObjectCodec a
required k c = Codec
    { decode = do
        m <- decode (optional k c)
        case m of
            Nothing ->
                Trans.lift
                    . Trans.throwE
                    $ "missing required member: "
                    <> show k
            Just x -> pure x
    , encode = \x -> do
        Monad.void . encode (optional k c) $ Just x
        pure x
    }

optional :: Name.Name -> ValueCodec a -> ObjectCodec (Maybe a)
optional k c = Codec
    { decode = do
        xs <- Trans.get
        case List.partition (\(Member.Member j _) -> j == k) xs of
            (Member.Member _ x : _, ys) -> case decodeWith c x of
                Left y -> Trans.lift $ Trans.throwE y
                Right y -> do
                    Trans.put ys
                    pure $ Just y
            _ -> pure Nothing
    , encode = \x -> do
        case x of
            Nothing -> pure ()
            Just y -> Trans.tell [Member.Member k $ encodeWith c y]
        pure x
    }
