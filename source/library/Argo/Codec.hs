module Argo.Codec where

import Control.Applicative ((<|>))

import qualified Argo.Json.Array as Array
import qualified Argo.Json.Boolean as Boolean
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Null as Null
import qualified Argo.Json.Number as Number
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Result as Result
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity
import qualified Data.Text as Text

decodeWith :: ValueCodec a -> Value.Value -> Result.Result a
decodeWith c = either fail pure
    . Identity.runIdentity
    . Trans.runExceptT
    . Trans.runReaderT (decode c)

encodeWith :: ValueCodec a -> a -> Value.Value
encodeWith c x = snd
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
    fmap f c = Codec
        { decode = fmap f $ decode c
        , encode = fmap f . encode c
        }

instance (Applicative r, Applicative w) => Applicative (CodecOf r w i) where
    pure x = Codec
        { decode = pure x
        , encode = const $ pure x
        }
    cf <*> cx = Codec
        { decode = decode cf <*> decode cx
        , encode = \ i -> encode cf i <*> encode cx i
        }

instance (Applicative.Alternative r, Applicative.Alternative w) => Applicative.Alternative (CodecOf r w i) where
    empty = Codec
        { decode = Applicative.empty
        , encode = const Applicative.empty
        }
    cx <|> cy = Codec
        { decode = decode cx <|> decode cy
        , encode = \ i -> encode cx i <|> encode cy i
        }

type Codec r w a = CodecOf r w a a

dimap :: (Functor r, Functor w) => (a -> b) -> (b -> a) -> Codec r w a -> Codec r w b
dimap f g c = Codec
    { decode = fmap f $ decode c
    , encode = fmap f . encode c . g
    }

type ValueCodec a = Codec
    (Trans.ReaderT Value.Value (Trans.ExceptT String Identity.Identity))
    (Trans.MaybeT (Trans.StateT Value.Value Identity.Identity))
    a

valueCodec :: ValueCodec Value.Value
valueCodec = Codec
    { decode = Trans.ask
    , encode = \ x -> do
        Trans.lift $ Trans.put x
        pure x
    }

nullCodec :: ValueCodec Null.Null
nullCodec = Codec
    { decode = do
        x <- Trans.ask
        case x of
            Value.Null y -> pure y
            _ -> Trans.lift . Trans.throwE $ "expected Null but got " <> show x
    , encode = \ x -> do
        Trans.lift . Trans.put $ Value.Null x
        pure x
    }

booleanCodec :: ValueCodec Boolean.Boolean
booleanCodec = Codec
    { decode = do
        x <- Trans.ask
        case x of
            Value.Boolean y -> pure y
            _ -> Trans.lift . Trans.throwE $ "expected Boolean but got " <> show x
    , encode = \ x -> do
        Trans.lift . Trans.put $ Value.Boolean x
        pure x
    }

numberCodec :: ValueCodec Number.Number
numberCodec = Codec
    { decode = do
        x <- Trans.ask
        case x of
            Value.Number y -> pure y
            _ -> Trans.lift . Trans.throwE $ "expected Number but got " <> show x
    , encode = \ x -> do
        Trans.lift . Trans.put $ Value.Number x
        pure x
    }

stringCodec :: ValueCodec String.String
stringCodec = Codec
    { decode = do
        x <- Trans.ask
        case x of
            Value.String y -> pure y
            _ -> Trans.lift . Trans.throwE $ "expected String but got " <> show x
    , encode = \ x -> do
        Trans.lift . Trans.put $ Value.String x
        pure x
    }

arrayCodec :: ValueCodec (Array.ArrayOf Value.Value)
arrayCodec = Codec
    { decode = do
        x <- Trans.ask
        case x of
            Value.Array y -> pure y
            _ -> Trans.lift . Trans.throwE $ "expected Array but got " <> show x
    , encode = \ x -> do
        Trans.lift . Trans.put $ Value.Array x
        pure x
    }

objectCodec :: ValueCodec (Object.ObjectOf Value.Value)
objectCodec = Codec
    { decode = do
        x <- Trans.ask
        case x of
            Value.Object y -> pure y
            _ -> Trans.lift . Trans.throwE $ "expected Object but got " <> show x
    , encode = \ x -> do
        Trans.lift . Trans.put $ Value.Object x
        pure x
    }

boolCodec :: ValueCodec Bool
boolCodec = dimap Boolean.toBool Boolean.fromBool booleanCodec

textCodec :: ValueCodec Text.Text
textCodec = dimap String.toText String.fromText stringCodec

maybeCodec :: ValueCodec a -> ValueCodec (Maybe a)
maybeCodec c =
    mapBoth Just id c
    <|> dimap (const Nothing) (const $ Null.fromUnit ()) nullCodec

eitherCodec :: ValueCodec a -> ValueCodec b -> ValueCodec (Either a b)
eitherCodec cx cy =
    mapBoth Left (either Just (const Nothing)) (tagged "Left" cx)
    <|> mapBoth Right (either (const Nothing) Just) (tagged "Right" cy)

mapBoth
    :: (Functor r, Applicative.Alternative w)
    => (o2 -> o1) -> (i1 -> Maybe i2) -> CodecOf r w i2 o2 -> CodecOf r w i1 o1
mapBoth f g c = Codec
    { decode = fmap f $ decode c
    , encode = \ x -> case g x of
        Nothing -> Applicative.empty
        Just y -> fmap f $ encode c y
    }

tagged :: String -> ValueCodec a -> ValueCodec a
tagged t c = dimap snd ((,) ()) . fromObjectCodec Allow $ (,)
    <$> project fst (required (Name.fromString . String.fromText $ Text.pack "type") (literalCodec (Value.String . String.fromText $ Text.pack t)))
    <*> project snd (required (Name.fromString . String.fromText $ Text.pack "value") c)

literalCodec :: Value.Value -> ValueCodec ()
literalCodec expected = Codec
    { decode = do
        actual <- Trans.ask
        Monad.when (actual /= expected)
            . Trans.lift
            . Trans.throwE
            $ "expected " <> show expected <> " but got " <> show actual
    , encode = const . Trans.lift $ Trans.put expected
    }

data Permission
    = Allow
    | Forbid
    deriving (Eq, Show)

type ListCodec e a = Codec
    (Trans.StateT [e] (Trans.ExceptT String Identity.Identity))
    (Trans.WriterT [e] Identity.Identity)
    a

fromListCodec :: ValueCodec [e] -> Permission -> ListCodec e a -> ValueCodec a
fromListCodec ce p ca = Codec
    { decode = do
        xs <- decode ce
        case Identity.runIdentity . Trans.runExceptT $ Trans.runStateT (decode ca) xs of
            Left x -> Trans.lift $ Trans.throwE x
            Right (x, ys) -> do
                case (p, ys) of
                    (Forbid, _ : _) -> Trans.lift $ Trans.throwE "leftover elements"
                    _ -> pure ()
                pure x
    , encode = \ x -> do
        Monad.void
            . encode ce
            . snd
            . Identity.runIdentity
            . Trans.runWriterT
            $ encode ca x
        pure x
    }

type ArrayCodec a = ListCodec Value.Value a

fromArrayCodec :: Permission -> ArrayCodec a -> ValueCodec a
fromArrayCodec = fromListCodec $ dimap Array.toList Array.fromList arrayCodec

element :: ValueCodec a -> ArrayCodec a
element c = Codec
    { decode = do
        l <- Trans.get
        case l of
            [] -> Trans.lift $ Trans.throwE "unexpected empty list"
            h : t ->  case decodeWith c h of
                Result.Failure y -> Trans.lift $ Trans.throwE y
                Result.Success y -> do
                    Trans.put t
                    pure y
    , encode = \ x -> do
        Trans.tell [encodeWith c x]
        pure x
    }

tupleCodec :: ValueCodec a -> ValueCodec b -> ValueCodec (a, b)
tupleCodec cx cy = fromArrayCodec Forbid $ (,)
    <$> project fst (element cx)
    <*> project snd (element cy)

type ObjectCodec a = ListCodec (Member.MemberOf Value.Value) a

fromObjectCodec :: Permission -> ObjectCodec a -> ValueCodec a
fromObjectCodec = fromListCodec $ dimap Object.toList Object.fromList objectCodec

required :: Name.Name -> ValueCodec a -> ObjectCodec a
required k c = Codec
    { decode = do
        m <- decode (optional k c)
        case m of
            Nothing -> Trans.lift . Trans.throwE $ "missing required member: " <> show k
            Just x -> pure x
    , encode = \ x -> do
        Monad.void . encode (optional k c) $ Just x
        pure x
    }

optional :: Name.Name -> ValueCodec a -> ObjectCodec (Maybe a)
optional k c = Codec
    { decode = do
        xs <- Trans.get
        case detect (\ (Member.Member j _) -> j == k) xs of
            Nothing -> pure Nothing
            Just (Member.Member _ x, ys) -> case decodeWith c x of
                Result.Failure y -> Trans.lift $ Trans.throwE y
                Result.Success y -> do
                    Trans.put ys
                    pure $ Just y
    , encode = \ x -> do
        case x of
            Nothing -> pure ()
            Just y -> Trans.tell [Member.Member k $ encodeWith c y]
        pure x
    }

detect :: (a -> Bool) -> [a] -> Maybe (a, [a])
detect = detectWith id

detectWith :: ([a] -> [a]) -> (a -> Bool) -> [a] -> Maybe (a, [a])
detectWith f p xs = case xs of
    [] -> Nothing
    x : ys -> if p x
        then Just (x, f ys)
        else detectWith (f . (x :)) p ys
