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
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Text as Text

decodeWith :: ValueCodec a -> Value.Value -> Result.Result a
decodeWith c = either fail pure . Except.runExcept . Reader.runReaderT (decode c)

encodeWith :: ValueCodec a -> a -> Value.Value
encodeWith c x = State.execState (encode c x) . Value.Null $ Null.fromUnit ()

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
    (Reader.ReaderT Value.Value (Except.Except String))
    (State.State Value.Value)
    a

valueCodec :: ValueCodec Value.Value
valueCodec = Codec
    { decode = Reader.ask
    , encode = \ x -> do
        State.put x
        pure x
    }

nullCodec :: ValueCodec Null.Null
nullCodec = Codec
    { decode = do
        x <- Reader.ask
        case x of
            Value.Null y -> pure y
            _ -> Trans.lift . Except.throwE $ "expected Null but got " <> show x
    , encode = \ x -> do
        State.put $ Value.Null x
        pure x
    }

booleanCodec :: ValueCodec Boolean.Boolean
booleanCodec = Codec
    { decode = do
        x <- Reader.ask
        case x of
            Value.Boolean y -> pure y
            _ -> Trans.lift . Except.throwE $ "expected Boolean but got " <> show x
    , encode = \ x -> do
        State.put $ Value.Boolean x
        pure x
    }

numberCodec :: ValueCodec Number.Number
numberCodec = Codec
    { decode = do
        x <- Reader.ask
        case x of
            Value.Number y -> pure y
            _ -> Trans.lift . Except.throwE $ "expected Number but got " <> show x
    , encode = \ x -> do
        State.put $ Value.Number x
        pure x
    }

stringCodec :: ValueCodec String.String
stringCodec = Codec
    { decode = do
        x <- Reader.ask
        case x of
            Value.String y -> pure y
            _ -> Trans.lift . Except.throwE $ "expected String but got " <> show x
    , encode = \ x -> do
        State.put $ Value.String x
        pure x
    }

arrayCodec :: ValueCodec (Array.ArrayOf Value.Value)
arrayCodec = Codec
    { decode = do
        x <- Reader.ask
        case x of
            Value.Array y -> pure y
            _ -> Trans.lift . Except.throwE $ "expected Array but got " <> show x
    , encode = \ x -> do
        State.put $ Value.Array x
        pure x
    }

objectCodec :: ValueCodec (Object.ObjectOf Value.Value)
objectCodec = Codec
    { decode = do
        x <- Reader.ask
        case x of
            Value.Object y -> pure y
            _ -> Trans.lift . Except.throwE $ "expected Object but got " <> show x
    , encode = \ x -> do
        State.put $ Value.Object x
        pure x
    }

boolCodec :: ValueCodec Bool
boolCodec = dimap Boolean.toBool Boolean.fromBool booleanCodec

textCodec :: ValueCodec Text.Text
textCodec = dimap String.toText String.fromText stringCodec

eitherCodec :: ValueCodec a -> ValueCodec b -> ValueCodec (Either a b)
eitherCodec c1 c2 = Codec
    { decode = fmap Left (decode c1) <|> fmap Right (decode c2)
    , encode = \ x -> case x of
        Left y -> fmap Left $ encode c1 y
        Right y -> fmap Right $ encode c2 y
    }

maybeCodec :: ValueCodec a -> ValueCodec (Maybe a)
maybeCodec =
    dimap
        (either Just $ const Nothing)
        (maybe (Right $ Null.fromUnit ()) Left)
    . flip eitherCodec nullCodec

data Permission
    = Allow
    | Forbid
    deriving (Eq, Show)

type ArrayCodec a = Codec
    (State.StateT [Value.Value] (Except.Except String))
    (Writer.Writer [Value.Value])
    a

fromArrayCodec :: Permission -> ArrayCodec a -> ValueCodec a
fromArrayCodec p c = Codec
    { decode = do
        xs <- decode arrayCodec
        case Except.runExcept . State.runStateT (decode c) $ Array.toList xs of
            Left x -> Trans.lift $ Except.throwE x
            Right (x, ys) -> do
                case (p, ys) of
                    (Forbid, _ : _) -> Trans.lift $ Except.throwE "leftover elements"
                    _ -> pure ()
                pure x
    , encode = \ x -> do
        Monad.void . encode arrayCodec . Array.fromList . Writer.execWriter $ encode c x
        pure x
    }

element :: ValueCodec a -> ArrayCodec a
element c = Codec
    { decode = do
        l <- State.get
        case l of
            [] -> Trans.lift $ Except.throwE "unexpected empty list"
            h : t ->  case decodeWith c h of
                Result.Failure y -> Trans.lift $ Except.throwE y
                Result.Success y -> do
                    State.put t
                    pure y
    , encode = \ x -> do
        Writer.tell [encodeWith c x]
        pure x
    }

tupleCodec :: ValueCodec a -> ValueCodec b -> ValueCodec (a, b)
tupleCodec cx cy = fromArrayCodec Forbid $ (,)
    <$> project fst (element cx)
    <*> project snd (element cy)

type ObjectCodec a = Codec
    (State.StateT [Member.MemberOf Value.Value] (Except.Except String))
    (Writer.Writer [Member.MemberOf Value.Value])
    a

fromObjectCodec :: Permission -> ObjectCodec a -> ValueCodec a
fromObjectCodec p c = Codec
    { decode = do
        xs <- decode objectCodec
        case Except.runExcept . State.runStateT (decode c) $ Object.toList xs of
            Left x -> Trans.lift $ Except.throwE x
            Right (x, ys) -> do
                case (p, ys) of
                    (Forbid, _ : _) -> Trans.lift $ Except.throwE "leftover members"
                    _ -> pure ()
                pure x
    , encode = \ x -> do
        Monad.void . encode objectCodec . Object.fromList . Writer.execWriter $ encode c x
        pure x
    }

required :: Name.Name -> ValueCodec a -> ObjectCodec a
required k c = Codec
    { decode = do
        m <- decode (optional k c)
        case m of
            Nothing -> Trans.lift . Except.throwE $ "missing required member: " <> show k
            Just x -> pure x
    , encode = \ x -> do
        Monad.void . encode (optional k c) $ Just x
        pure x
    }

optional :: Name.Name -> ValueCodec a -> ObjectCodec (Maybe a)
optional k c = Codec
    { decode = do
        xs <- State.get
        case detect (\ (Member.Member j _) -> j == k) xs of
            Nothing -> pure Nothing
            Just (Member.Member _ x, ys) -> case decodeWith c x of
                Result.Failure y -> Trans.lift $ Except.throwE y
                Result.Success y -> do
                    State.put ys
                    pure $ Just y
    , encode = \ x -> do
        case x of
            Nothing -> pure ()
            Just y -> Writer.tell [Member.Member k $ encodeWith c y]
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
