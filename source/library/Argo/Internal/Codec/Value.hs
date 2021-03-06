{-# LANGUAGE ScopedTypeVariables #-}

module Argo.Internal.Codec.Value where

import qualified Argo.Internal.Codec.Codec as Codec
import qualified Argo.Internal.Json.Array as Array
import qualified Argo.Internal.Json.Null as Null
import qualified Argo.Internal.Json.Object as Object
import qualified Argo.Internal.Json.Value as Value
import qualified Argo.Internal.Schema.Identifier as Identifier
import qualified Argo.Internal.Schema.Schema as Schema
import qualified Argo.Vendor.Map as Map
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity
import qualified Data.Typeable as Typeable

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
          ( Trans.AccumT
                (Map.Map Identifier.Identifier Schema.Schema)
                Identity.Identity
                (Maybe Identifier.Identifier, Schema.Schema)
          )
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
    , Codec.schema = pure $ Schema.unidentified Schema.false
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
    , Codec.schema = pure $ Schema.unidentified Schema.false
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
    , Codec.schema = pure . Schema.unidentified $ Schema.Const expected
    }

identified :: forall a . Typeable.Typeable a => Value a -> Value a
identified = withIdentifier . Identifier.fromText . Text.pack $ typeName
    (Typeable.Proxy :: Typeable.Proxy a)

withIdentifier :: Identifier.Identifier -> Value a -> Value a
withIdentifier identifier codec =
    let
        newSchema = do
            (_, schema) <- Codec.schema codec
            Trans.add $ Map.singleton identifier schema
            pure $ Schema.withIdentifier identifier schema
    in codec { Codec.schema = newSchema }

typeName :: Typeable.Typeable a => Typeable.Proxy a -> String
typeName = show . Typeable.typeRep

getRef
    :: Value a
    -> Trans.AccumT
           (Map.Map Identifier.Identifier Schema.Schema)
           Identity.Identity
           Schema.Schema
getRef codec = do
    let
        (maybeIdentifier, schema) =
            fst . Identity.runIdentity $ Trans.runAccumT
                (Codec.schema codec)
                Map.empty
    case maybeIdentifier of
        Nothing -> pure schema
        Just identifier -> do
            schemas <- Trans.look
            Monad.unless (Map.member identifier schemas) $ do
                Trans.add $ Map.singleton identifier schema
                Monad.void $ Codec.schema codec
            pure $ Schema.Ref identifier
