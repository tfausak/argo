module Argo.Codec.Value where

import qualified Argo.Codec.Codec as Codec
import qualified Argo.Json.Array as Array
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Null as Null
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Schema.Schema as Schema
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity

decodeWith :: Value a -> Value.Value -> Either String a
decodeWith c =
    Identity.runIdentity . Trans.runExceptT . Trans.runReaderT (Codec.decode c)

encodeWith :: Value a -> a -> Value.Value
encodeWith c x =
    snd
        . Identity.runIdentity
        . Trans.runStateT (Trans.runMaybeT $ Codec.encode c x)
        . Value.Null
        $ Null.fromUnit ()

type Value a
    = Codec.Codec
          (Trans.ReaderT Value.Value (Trans.ExceptT String Identity.Identity))
          (Trans.MaybeT (Trans.StateT Value.Value Identity.Identity))
          Schema.Schema
          a
          a

arrayCodec :: Value (Array.Array Value.Value)
arrayCodec = Codec.Codec
    { Codec.decode = do
        x <- Trans.ask
        case x of
            Value.Array y -> pure y
            _ ->
                Trans.lift . Trans.throwE $ "expected Array but got " <> show x
    , Codec.encode = \x -> do
        Trans.lift . Trans.put $ Value.Array x
        pure x
    , Codec.schema = Schema.false
    }

objectCodec :: Value (Object.Object Value.Value)
objectCodec = Codec.Codec
    { Codec.decode = do
        x <- Trans.ask
        case x of
            Value.Object y -> pure y
            _ ->
                Trans.lift
                    . Trans.throwE
                    $ "expected Object but got "
                    <> show x
    , Codec.encode = \x -> do
        Trans.lift . Trans.put $ Value.Object x
        pure x
    , Codec.schema = Schema.false
    }

literalCodec :: Value.Value -> Value ()
literalCodec expected = Codec.Codec
    { Codec.decode = do
        actual <- Trans.ask
        Monad.when (actual /= expected)
            . Trans.lift
            . Trans.throwE
            $ "expected "
            <> show expected
            <> " but got "
            <> show actual
    , Codec.encode = const . Trans.lift $ Trans.put expected
    , Codec.schema = Schema.fromValue . Value.Object $ Object.fromList
        [ Member.fromTuple
              (Name.fromString . String.fromText $ Text.pack "const", expected)
        ]
    }
