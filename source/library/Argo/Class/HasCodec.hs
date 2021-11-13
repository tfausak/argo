{-# LANGUAGE FlexibleInstances #-}

module Argo.Class.HasCodec where

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
import qualified Argo.Type.Codec as Codec
import qualified Argo.Type.Decimal as Decimal
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.Map as Map
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.Functor.Identity as Identity

class HasCodec a where
    codec :: Codec.ValueCodec a

instance HasCodec Value.Value where
    codec = Codec.Codec
        { Codec.decode = Trans.ask
        , Codec.encode = Codec.tap $ Trans.lift . Trans.put
        }

instance HasCodec Null.Null where
    codec = basicCodec "Null" Value.Null $ \ value -> case value of
        Value.Null null_ -> Just null_
        _ -> Nothing

instance HasCodec Boolean.Boolean where
    codec = basicCodec "Boolean" Value.Boolean $ \ value -> case value of
        Value.Boolean boolean -> Just boolean
        _ -> Nothing

instance HasCodec Number.Number where
    codec = basicCodec "Number" Value.Number $ \ value -> case value of
        Value.Number number -> Just number
        _ -> Nothing

instance HasCodec String.String where
    codec = basicCodec "String" Value.String $ \ value -> case value of
        Value.String string -> Just string
        _ -> Nothing

instance HasCodec a => HasCodec (Array.ArrayOf a) where
    codec = Codec.Codec
        { Codec.decode = do
            array <- castValue "Array" $ \ value -> case value of
                Value.Array array -> Just array
                _ -> Nothing
            either (Trans.lift . Trans.throwE) (pure . Array.fromList)
                . traverse (Codec.decodeWith codec)
                $ Array.toList array
        , Codec.encode = Codec.tap
            $ Trans.lift
            . Trans.put
            . Value.Array
            . Array.fromList
            . fmap (Codec.encodeWith codec)
            . Array.toList
        }

instance HasCodec a => HasCodec (Object.ObjectOf a) where
    codec = Codec.Codec
        { Codec.decode = do
            object <- castValue "Object" $ \ value -> case value of
                Value.Object object -> Just object
                _ -> Nothing
            either (Trans.lift . Trans.throwE) (pure . Object.fromList)
                . traverse (\ (Member.Member k v) -> Member.Member k <$> Codec.decodeWith codec v)
                $ Object.toList object
        , Codec.encode = Codec.tap
            $ Trans.lift
            . Trans.put
            . Value.Object
            . Object.fromList
            . fmap (\ (Member.Member k v) -> Member.Member k $ Codec.encodeWith codec v)
            . Object.toList
        }

instance HasCodec a => HasCodec (Maybe a) where
    codec = Codec.mapBoth Just id codec
        <|> Codec.dimap (const Nothing) (const $ Null.fromUnit ()) codec

instance (HasCodec a, HasCodec b) => HasCodec (Either a b) where
    codec = Codec.mapBoth Left (either Just $ const Nothing) (Codec.tagged "Left" codec)
        <|> Codec.mapBoth Right (either (const Nothing) Just) (Codec.tagged "Right" codec)

instance HasCodec () where
    codec = Codec.fromArrayCodec Permission.Forbid $ pure ()

instance (HasCodec a, HasCodec b) => HasCodec (a, b) where
    codec = Codec.fromArrayCodec Permission.Forbid $ (,)
        <$> Codec.project fst (Codec.element codec)
        <*> Codec.project snd (Codec.element codec)

instance HasCodec Bool where
    codec = Codec.dimap Boolean.toBool Boolean.fromBool codec

instance HasCodec Decimal.Decimal where
    codec = Codec.dimap Number.toDecimal Number.fromDecimal codec

instance HasCodec Text.Text where
    codec = Codec.dimap String.toText String.fromText codec

instance HasCodec a => HasCodec [a] where
    codec = Codec.dimap Array.toList Array.fromList codec

instance HasCodec a => HasCodec (Map.Map Name.Name a) where
    codec = Codec.dimap
        (Map.fromList . fmap Member.toTuple . Object.toList)
        (Object.fromList . fmap Member.fromTuple . Map.toList)
        codec

basicCodec :: String -> (a -> Value.Value) -> (Value.Value -> Maybe a) -> Codec.ValueCodec a
basicCodec expected toValue fromValue = Codec.Codec
    { Codec.decode = castValue expected fromValue
    , Codec.encode = Codec.tap $ Trans.lift . Trans.put . toValue
    }

castValue
    :: String
    -> (Value.Value -> Maybe a)
    -> Trans.ReaderT Value.Value (Trans.ExceptT String Identity.Identity) a
castValue expected fromValue = do
    value <- Trans.ask
    case fromValue value of
        Nothing -> Trans.lift . Trans.throwE $ typeMismatch expected value
        Just x -> pure x

typeMismatch :: String -> Value.Value -> String
typeMismatch expected value =
    let
        actual = case value of
            Value.Null _ -> "Null"
            Value.Boolean _ -> "Boolean"
            Value.Number _ -> "Number"
            Value.String _ -> "String"
            Value.Array _ -> "Array"
            Value.Object _ -> "Object"
    in "expected " <> expected <> " but got " <> actual
