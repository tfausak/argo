{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Argo.Class.HasCodec where

import Control.Applicative ((<|>))

import qualified Argo.Codec.Array as Codec
import qualified Argo.Codec.Codec as Codec
import qualified Argo.Codec.Object as Codec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Json.Array as Array
import qualified Argo.Json.Boolean as Boolean
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Null as Null
import qualified Argo.Json.Number as Number
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Schema.Schema as Schema
import qualified Argo.Type.Config as Config
import qualified Argo.Type.Decimal as Decimal
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Type.Encoder as Encoder
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.ByteString as ByteString
import qualified Argo.Vendor.Map as Map
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.Bits as Bits
import qualified Data.Functor.Identity as Identity
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Word as Word

class HasCodec a where
    codec :: Codec.Value a

instance HasCodec Value.Value where
    codec = Codec.Codec
        { Codec.decode = Trans.ask
        , Codec.encode = Codec.tap $ Trans.lift . Trans.put
        , Codec.schema = Schema.true
        }

instance HasCodec Null.Null where
    codec =
        let
            schema = Schema.fromValue . Value.Object $ Object.fromList
                [ Member.fromTuple
                      ( Name.fromString . String.fromText $ Text.pack "type"
                      , Value.String . String.fromText $ Text.pack "null"
                      )
                ]
        in
            basicCodec "Null" schema Value.Null $ \value -> case value of
                Value.Null null_ -> Just null_
                _ -> Nothing

instance HasCodec Boolean.Boolean where
    codec =
        let
            schema = Schema.fromValue . Value.Object $ Object.fromList
                [ Member.fromTuple
                      ( Name.fromString . String.fromText $ Text.pack "type"
                      , Value.String . String.fromText $ Text.pack "boolean"
                      )
                ]
        in
            basicCodec "Boolean" schema Value.Boolean $ \value -> case value of
                Value.Boolean boolean -> Just boolean
                _ -> Nothing

instance HasCodec Number.Number where
    codec =
        let
            schema = Schema.fromValue . Value.Object $ Object.fromList
                [ Member.fromTuple
                      ( Name.fromString . String.fromText $ Text.pack "type"
                      , Value.String . String.fromText $ Text.pack "number"
                      )
                ]
        in
            basicCodec "Number" schema Value.Number $ \value -> case value of
                Value.Number number -> Just number
                _ -> Nothing

instance HasCodec String.String where
    codec =
        let
            schema = Schema.fromValue . Value.Object $ Object.fromList
                [ Member.fromTuple
                      ( Name.fromString . String.fromText $ Text.pack "type"
                      , Value.String . String.fromText $ Text.pack "string"
                      )
                ]
        in
            basicCodec "String" schema Value.String $ \value -> case value of
                Value.String string -> Just string
                _ -> Nothing

instance HasCodec a => HasCodec (Array.Array a) where
    codec = Codec.Codec
        { Codec.decode = do
            array <- castValue "Array" $ \value -> case value of
                Value.Array array -> Just array
                _ -> Nothing
            either (Trans.lift . Trans.throwE) (pure . Array.fromList)
                . traverse (Codec.decodeWith codec)
                $ Array.toList array
        , Codec.encode =
            Codec.tap
            $ Trans.lift
            . Trans.put
            . Value.Array
            . Array.fromList
            . fmap (Codec.encodeWith codec)
            . Array.toList
        , Codec.schema = Schema.fromValue . Value.Object $ Object.fromList
            [ Member.fromTuple
                ( Name.fromString . String.fromText $ Text.pack "type"
                , Value.String . String.fromText $ Text.pack "array"
                )
            , Member.fromTuple
                ( Name.fromString . String.fromText $ Text.pack "items"
                , Schema.toValue $ Codec.schema (codec :: Codec.Value a)
                )
            ]
        }

instance HasCodec a => HasCodec (Object.Object a) where
    codec = Codec.Codec
        { Codec.decode = do
            object <- castValue "Object" $ \value -> case value of
                Value.Object object -> Just object
                _ -> Nothing
            either (Trans.lift . Trans.throwE) (pure . Object.fromList)
                . traverse
                      (\(Member.Member k v) ->
                          Member.Member k <$> Codec.decodeWith codec v
                      )
                $ Object.toList object
        , Codec.encode =
            Codec.tap
            $ Trans.lift
            . Trans.put
            . Value.Object
            . Object.fromList
            . fmap
                  (\(Member.Member k v) ->
                      Member.Member k $ Codec.encodeWith codec v
                  )
            . Object.toList
        , Codec.schema = Schema.comment "TODO: Object a"
        }

instance HasCodec a => HasCodec (Maybe a) where
    codec =
        Codec.mapMaybe (Just . Just) id codec
            <|> Codec.map (const Nothing) (const $ Null.fromUnit ()) codec

instance (HasCodec a, HasCodec b) => HasCodec (Either a b) where
    codec =
        Codec.mapMaybe
                (Just . Left)
                (either Just $ const Nothing)
                (Codec.tagged "Left" codec)
            <|> Codec.mapMaybe
                    (Just . Right)
                    (either (const Nothing) Just)
                    (Codec.tagged "Right" codec)

instance HasCodec () where
    codec = Codec.fromArrayCodec Permission.Forbid $ pure ()

instance (HasCodec a, HasCodec b) => HasCodec (a, b) where
    codec =
        Codec.fromArrayCodec Permission.Forbid
            $ (,)
            <$> Codec.project fst (Codec.element codec)
            <*> Codec.project snd (Codec.element codec)

instance HasCodec Bool where
    codec = Codec.map Boolean.toBool Boolean.fromBool codec

instance HasCodec Decimal.Decimal where
    codec = Codec.map Number.toDecimal Number.fromDecimal codec

instance HasCodec Text.Text where
    codec = Codec.map String.toText String.fromText codec

instance {-# OVERLAPPABLE #-} HasCodec a => HasCodec [a] where
    codec = Codec.map Array.toList Array.fromList codec

instance HasCodec a => HasCodec (Map.Map Name.Name a) where
    codec = Codec.map
        (Map.fromList . fmap Member.toTuple . Object.toList)
        (Object.fromList . fmap Member.fromTuple . Map.toList)
        codec

instance HasCodec String where
    codec = Codec.map Text.unpack Text.pack codec

instance HasCodec Char where
    codec = Codec.mapMaybe
        (\x -> case Text.uncons x of
            Just (y, z) | Text.null z -> Just y
            _ -> Nothing
        )
        (Just . Text.singleton)
        codec

instance HasCodec Text.LazyText where
    codec = Codec.map Text.fromStrict Text.toStrict codec

instance HasCodec a => HasCodec (NonEmpty.NonEmpty a) where
    codec = Codec.mapMaybe NonEmpty.nonEmpty (Just . NonEmpty.toList) codec

instance HasCodec Integer where
    codec =
        Codec.mapMaybe Decimal.toInteger (Just . Decimal.fromInteger) codec

instance HasCodec Int where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Int
            into = fromIntegral :: Int -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Int.Int8 where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Int.Int8
            into = fromIntegral :: Int.Int8 -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Int.Int16 where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Int.Int16
            into = fromIntegral :: Int.Int16 -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Int.Int32 where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Int.Int32
            into = fromIntegral :: Int.Int32 -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Int.Int64 where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Int.Int64
            into = fromIntegral :: Int.Int64 -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Word where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Word
            into = fromIntegral :: Word -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Word.Word8 where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Word.Word8
            into = fromIntegral :: Word.Word8 -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Word.Word16 where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Word.Word16
            into = fromIntegral :: Word.Word16 -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Word.Word32 where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Word.Word32
            into = fromIntegral :: Word.Word32 -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Word.Word64 where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Word.Word64
            into = fromIntegral :: Word.Word64 -> Integer
        in Codec.mapMaybe from (Just . into) codec

instance HasCodec Float where
    codec =
        Codec.mapMaybe (Just . Decimal.toRealFloat) Decimal.fromRealFloat codec

instance HasCodec Double where
    codec =
        Codec.mapMaybe (Just . Decimal.toRealFloat) Decimal.fromRealFloat codec

instance HasCodec Pointer.Pointer where
    codec = Codec.mapMaybe
        (either (const Nothing) Just
        . Decoder.run Pointer.decode
        . Text.encodeUtf8
        )
        (either (const Nothing) Just
        . Text.decodeUtf8'
        . ByteString.toStrict
        . Builder.toLazyByteString
        . Encoder.run Config.initial
        . Pointer.encode
        )
        codec

instance HasCodec Schema.Schema where
    codec = Codec.map Schema.fromValue Schema.toValue codec

basicCodec
    :: String
    -> Schema.Schema
    -> (a -> Value.Value)
    -> (Value.Value -> Maybe a)
    -> Codec.Value a
basicCodec expected schema toValue fromValue = Codec.Codec
    { Codec.decode = castValue expected fromValue
    , Codec.encode = Codec.tap $ Trans.lift . Trans.put . toValue
    , Codec.schema = schema
    }

castValue
    :: String
    -> (Value.Value -> Maybe a)
    -> Trans.ReaderT
           Value.Value
           (Trans.ExceptT String Identity.Identity)
           a
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
