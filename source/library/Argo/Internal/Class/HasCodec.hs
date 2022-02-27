{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Argo.Internal.Class.HasCodec where

import Control.Applicative ((<|>))

import qualified Argo.Internal.Codec.Array as Codec
import qualified Argo.Internal.Codec.Codec as Codec
import qualified Argo.Internal.Codec.Object as Codec
import qualified Argo.Internal.Codec.Value as Codec
import qualified Argo.Internal.Json.Array as Array
import qualified Argo.Internal.Json.Boolean as Boolean
import qualified Argo.Internal.Json.Member as Member
import qualified Argo.Internal.Json.Name as Name
import qualified Argo.Internal.Json.Null as Null
import qualified Argo.Internal.Json.Number as Number
import qualified Argo.Internal.Json.Object as Object
import qualified Argo.Internal.Json.String as String
import qualified Argo.Internal.Json.Value as Value
import qualified Argo.Internal.Pointer.Pointer as Pointer
import qualified Argo.Internal.Schema.Identifier as Identifier
import qualified Argo.Internal.Schema.Schema as Schema
import qualified Argo.Internal.Type.Config as Config
import qualified Argo.Internal.Type.Decimal as Decimal
import qualified Argo.Internal.Type.Decoder as Decoder
import qualified Argo.Internal.Type.Encoder as Encoder
import qualified Argo.Internal.Type.Nullable as Nullable
import qualified Argo.Internal.Type.Optional as Optional
import qualified Argo.Internal.Type.Permission as Permission
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.ByteString as ByteString
import qualified Argo.Vendor.Map as Map
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.Bits as Bits
import qualified Data.Functor.Identity as Identity
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified Data.Typeable as Typeable
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural

class Typeable.Typeable a => HasCodec a where
    codec :: Codec.Value a

instance HasCodec Value.Value where
    codec = valueCodec Schema.true id Just

instance HasCodec Null.Null where
    codec =
        let schema = Schema.Null
        in
            valueCodec schema Value.Null $ \value -> case value of
                Value.Null null_ -> Just null_
                _ -> Nothing

instance HasCodec Boolean.Boolean where
    codec =
        let schema = Schema.Boolean
        in
            valueCodec schema Value.Boolean $ \value -> case value of
                Value.Boolean boolean -> Just boolean
                _ -> Nothing

instance HasCodec Number.Number where
    codec =
        let schema = Schema.Number
        in
            valueCodec schema Value.Number $ \value -> case value of
                Value.Number number -> Just number
                _ -> Nothing

instance HasCodec String.String where
    codec =
        let schema = Schema.String Nothing Nothing
        in
            valueCodec schema Value.String $ \value -> case value of
                Value.String string -> Just string
                _ -> Nothing

instance HasCodec a => HasCodec (Array.Array a) where
    codec = Codec.identified Codec.Codec
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
        , Codec.schema = do
            ref <- Codec.getRef (codec :: Codec.Value a)
            pure . Schema.unidentified $ Schema.Array
                Nothing
                Nothing
                (Left ref)
                Nothing
        }

instance HasCodec a => HasCodec (Object.Object a) where
    codec = Codec.identified Codec.Codec
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
        , Codec.schema = do
            ref <- Codec.getRef (codec :: Codec.Value a)
            pure . Schema.unidentified . Schema.Object [] [] $ Just ref
        }

instance HasCodec a => HasCodec (Nullable.Nullable a) where
    codec = nullable codec

instance HasCodec a => HasCodec (Maybe a) where
    codec =
        Codec.identified $ Codec.map Nullable.toMaybe Nullable.fromMaybe codec

instance
    ( HasCodec a
    , HasCodec b
    ) => HasCodec (Either a b) where
    codec =
        Codec.identified
            $ Codec.mapMaybe
                  (Just . Left)
                  (either Just $ const Nothing)
                  (Codec.tagged "Left" codec)
            <|> Codec.mapMaybe
                    (Just . Right)
                    (either (const Nothing) Just)
                    (Codec.tagged "Right" codec)

instance HasCodec () where
    codec =
        Codec.identified . Codec.fromArrayCodec Permission.Forbid $ pure ()

instance
    ( HasCodec a
    , HasCodec b
    ) => HasCodec (a, b) where
    codec =
        Codec.identified
            . Codec.fromArrayCodec Permission.Forbid
            $ (,)
            <$> Codec.project fst (Codec.element codec)
            <*> Codec.project snd (Codec.element codec)

instance
    ( HasCodec a
    , HasCodec b
    , HasCodec c
    ) => HasCodec (a, b, c) where
    codec =
        Codec.identified
            . Codec.fromArrayCodec Permission.Forbid
            $ (,,)
            <$> Codec.project (\(a, _, _) -> a) (Codec.element codec)
            <*> Codec.project (\(_, b, _) -> b) (Codec.element codec)
            <*> Codec.project (\(_, _, c) -> c) (Codec.element codec)

instance
    ( HasCodec a
    , HasCodec b
    , HasCodec c
    , HasCodec d
    ) => HasCodec (a, b, c, d) where
    codec =
        Codec.identified
            . Codec.fromArrayCodec Permission.Forbid
            $ (,,,)
            <$> Codec.project (\(a, _, _, _) -> a) (Codec.element codec)
            <*> Codec.project (\(_, b, _, _) -> b) (Codec.element codec)
            <*> Codec.project (\(_, _, c, _) -> c) (Codec.element codec)
            <*> Codec.project (\(_, _, _, d) -> d) (Codec.element codec)

instance
    ( HasCodec a
    , HasCodec b
    , HasCodec c
    , HasCodec d
    , HasCodec e
    ) => HasCodec (a, b, c, d, e) where
    codec =
        Codec.identified
            . Codec.fromArrayCodec Permission.Forbid
            $ (,,,,)
            <$> Codec.project (\(a, _, _, _, _) -> a) (Codec.element codec)
            <*> Codec.project (\(_, b, _, _, _) -> b) (Codec.element codec)
            <*> Codec.project (\(_, _, c, _, _) -> c) (Codec.element codec)
            <*> Codec.project (\(_, _, _, d, _) -> d) (Codec.element codec)
            <*> Codec.project (\(_, _, _, _, e) -> e) (Codec.element codec)

instance
    ( HasCodec a
    , HasCodec b
    , HasCodec c
    , HasCodec d
    , HasCodec e
    , HasCodec f
    ) => HasCodec (a, b, c, d, e, f) where
    codec =
        Codec.identified
            . Codec.fromArrayCodec Permission.Forbid
            $ (,,,,,)
            <$> Codec.project (\(a, _, _, _, _, _) -> a) (Codec.element codec)
            <*> Codec.project (\(_, b, _, _, _, _) -> b) (Codec.element codec)
            <*> Codec.project (\(_, _, c, _, _, _) -> c) (Codec.element codec)
            <*> Codec.project (\(_, _, _, d, _, _) -> d) (Codec.element codec)
            <*> Codec.project (\(_, _, _, _, e, _) -> e) (Codec.element codec)
            <*> Codec.project (\(_, _, _, _, _, f) -> f) (Codec.element codec)

instance
    ( HasCodec a
    , HasCodec b
    , HasCodec c
    , HasCodec d
    , HasCodec e
    , HasCodec f
    , HasCodec g
    ) => HasCodec (a, b, c, d, e, f, g) where
    codec =
        Codec.identified
            . Codec.fromArrayCodec Permission.Forbid
            $ (,,,,,,)
            <$> Codec.project
                    (\(a, _, _, _, _, _, _) -> a)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, b, _, _, _, _, _) -> b)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, c, _, _, _, _) -> c)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, _, d, _, _, _) -> d)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, _, _, e, _, _) -> e)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, _, _, _, f, _) -> f)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, _, _, _, _, g) -> g)
                    (Codec.element codec)

instance
    ( HasCodec a
    , HasCodec b
    , HasCodec c
    , HasCodec d
    , HasCodec e
    , HasCodec f
    , HasCodec g
    , HasCodec h
    ) => HasCodec (a, b, c, d, e, f, g, h) where
    codec =
        Codec.identified
            . Codec.fromArrayCodec Permission.Forbid
            $ (,,,,,,,)
            <$> Codec.project
                    (\(a, _, _, _, _, _, _, _) -> a)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, b, _, _, _, _, _, _) -> b)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, c, _, _, _, _, _) -> c)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, _, d, _, _, _, _) -> d)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, _, _, e, _, _, _) -> e)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, _, _, _, f, _, _) -> f)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, _, _, _, _, g, _) -> g)
                    (Codec.element codec)
            <*> Codec.project
                    (\(_, _, _, _, _, _, _, h) -> h)
                    (Codec.element codec)

instance HasCodec Bool where
    codec = Codec.identified $ Codec.map Boolean.toBool Boolean.fromBool codec

instance HasCodec Decimal.Decimal where
    codec =
        Codec.identified $ Codec.map Number.toDecimal Number.fromDecimal codec

instance HasCodec Text.Text where
    codec = Codec.identified $ Codec.map String.toText String.fromText codec

instance {-# OVERLAPPABLE #-} HasCodec a => HasCodec [a] where
    codec = Codec.identified $ Codec.map Array.toList Array.fromList codec

instance HasCodec a => HasCodec (Map.Map Name.Name a) where
    codec = Codec.identified $ Codec.map
        (Map.fromList . fmap Member.toTuple . Object.toList)
        (Object.fromList . fmap Member.fromTuple . Map.toList)
        codec

instance HasCodec a => HasCodec (Map.Map String.String a) where
    codec = Codec.identified $ Codec.map
        (Map.mapKeys Name.toString)
        (Map.mapKeys Name.fromString)
        codec

instance HasCodec a => HasCodec (Map.Map Text.Text a) where
    codec = Codec.identified $ Codec.map
        (Map.mapKeys String.toText)
        (Map.mapKeys String.fromText)
        codec

instance HasCodec a => HasCodec (Map.Map Text.LazyText a) where
    codec = Codec.identified $ Codec.map
        (Map.mapKeys Text.fromStrict)
        (Map.mapKeys Text.toStrict)
        codec

instance HasCodec a => HasCodec (Map.Map String a) where
    codec = Codec.identified
        $ Codec.map (Map.mapKeys Text.unpack) (Map.mapKeys Text.pack) codec

instance HasCodec String where
    codec = Codec.identified $ Codec.map Text.unpack Text.pack codec

instance HasCodec Char where
    codec =
        let schema = Schema.unidentified $ Schema.String (Just 1) (Just 1)
        in
            Codec.identified $ Codec.mapMaybe
                (\x -> case Text.uncons x of
                    Just (y, z) | Text.null z -> Just y
                    _ -> Nothing
                )
                (Just . Text.singleton)
                codec { Codec.schema = pure schema }

instance HasCodec Text.LazyText where
    codec = Codec.identified $ Codec.map Text.fromStrict Text.toStrict codec

instance HasCodec a => HasCodec (NonEmpty.NonEmpty a) where
    codec =
        let
            schema = do
                itemSchema <- Codec.schema (codec :: Codec.Value a)
                pure . Schema.unidentified $ Schema.Array
                    (Just 1)
                    Nothing
                    (Left $ Schema.maybeRef itemSchema)
                    Nothing
        in
            Codec.identified $ Codec.mapMaybe
                NonEmpty.nonEmpty
                (Just . NonEmpty.toList)
                codec { Codec.schema = schema }

instance HasCodec Integer where
    codec =
        let schema = Schema.unidentified $ Schema.Integer Nothing Nothing
        in
            Codec.identified $ Codec.mapMaybe
                Decimal.toInteger
                (Just . Decimal.fromInteger)
                codec { Codec.schema = pure schema }

instance HasCodec Int where
    codec = integralCodec

instance HasCodec Int.Int8 where
    codec = integralCodec

instance HasCodec Int.Int16 where
    codec = integralCodec

instance HasCodec Int.Int32 where
    codec = integralCodec

instance HasCodec Int.Int64 where
    codec = integralCodec

instance HasCodec Natural.Natural where
    codec =
        let
            from = Bits.toIntegralSized :: Integer -> Maybe Natural.Natural
            into = fromIntegral :: Natural.Natural -> Integer
            schema = Schema.unidentified $ Schema.Integer (Just 0) Nothing
        in Codec.identified $ Codec.mapMaybe
            from
            (Just . into)
            codec { Codec.schema = pure schema }

instance HasCodec Word where
    codec = integralCodec

instance HasCodec Word.Word8 where
    codec = integralCodec

instance HasCodec Word.Word16 where
    codec = integralCodec

instance HasCodec Word.Word32 where
    codec = integralCodec

instance HasCodec Word.Word64 where
    codec = integralCodec

instance HasCodec Float where
    codec = Codec.identified $ Codec.mapMaybe
        (Just . Decimal.toRealFloat)
        Decimal.fromRealFloat
        codec

instance HasCodec Double where
    codec = Codec.identified $ Codec.mapMaybe
        (Just . Decimal.toRealFloat)
        Decimal.fromRealFloat
        codec

instance HasCodec Pointer.Pointer where
    codec = Codec.identified $ Codec.mapMaybe
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

instance HasCodec Identifier.Identifier where
    codec =
        let prefix = Text.pack "#/definitions/"
        in
            Codec.identified $ Codec.mapMaybe
                (fmap Identifier.fromText . Text.stripPrefix prefix)
                (Just . mappend prefix . Identifier.toText)
                codec

instance HasCodec Name.Name where
    codec = Codec.identified $ Codec.map Name.fromString Name.toString codec

instance HasCodec Schema.Schema where
    codec =
        let
            trueCodec =
                Codec.mapMaybe
                        (const $ Just Schema.True)
                        (\x -> case x of
                            Schema.True -> Just ()
                            _ -> Nothing
                        )
                    . Codec.literalCodec
                    . Value.Boolean
                    $ Boolean.fromBool True
            falseCodec =
                Codec.mapMaybe
                        (const $ Just Schema.False)
                        (\x -> case x of
                            Schema.False -> Just ()
                            _ -> Nothing
                        )
                    . Codec.literalCodec
                    . Value.Boolean
                    $ Boolean.fromBool False
            constCodec =
                Codec.mapMaybe
                        (Just . Schema.Const)
                        (\x -> case x of
                            Schema.Const y -> Just y
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    $ Codec.required (name "const") codec
            nullCodec =
                Codec.mapMaybe
                        (const $ Just Schema.Null)
                        (\x -> case x of
                            Schema.Null -> Just ()
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    . Codec.required (name "type")
                    . Codec.literalCodec
                    . Value.String
                    . String.fromText
                    $ Text.pack "null"
            booleanCodec =
                Codec.mapMaybe
                        (const $ Just Schema.Boolean)
                        (\x -> case x of
                            Schema.Boolean -> Just ()
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    . Codec.required (name "type")
                    . Codec.literalCodec
                    . Value.String
                    . String.fromText
                    $ Text.pack "boolean"
            numberCodec =
                Codec.mapMaybe
                        (const $ Just Schema.Number)
                        (\x -> case x of
                            Schema.Number -> Just ()
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    . Codec.required (name "type")
                    . Codec.literalCodec
                    . Value.String
                    . String.fromText
                    $ Text.pack "number"
            integerCodec =
                Codec.mapMaybe
                        (\(_, y, z) -> Just $ Schema.Integer
                            (Optional.toMaybe y)
                            (Optional.toMaybe z)
                        )
                        (\x -> case x of
                            Schema.Integer y z ->
                                Just
                                    ( ()
                                    , Optional.fromMaybe y
                                    , Optional.fromMaybe z
                                    )
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    $ (,,)
                    <$> Codec.project
                            (\(a, _, _) -> a)
                            (Codec.required (name "type")
                            . Codec.literalCodec
                            . Value.String
                            . String.fromText
                            $ Text.pack "integer"
                            )
                    <*> Codec.project
                            (\(_, b, _) -> b)
                            (Codec.optional (name "minimum") codec)
                    <*> Codec.project
                            (\(_, _, c) -> c)
                            (Codec.optional (name "maximum") codec)
            stringCodec =
                Codec.mapMaybe
                        (\(_, y, z) -> Just $ Schema.String
                            (Optional.toMaybe y)
                            (Optional.toMaybe z)
                        )
                        (\x -> case x of
                            Schema.String y z ->
                                Just
                                    ( ()
                                    , Optional.fromMaybe y
                                    , Optional.fromMaybe z
                                    )
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    $ (,,)
                    <$> Codec.project
                            (\(a, _, _) -> a)
                            (Codec.required (name "type")
                            . Codec.literalCodec
                            . Value.String
                            . String.fromText
                            $ Text.pack "string"
                            )
                    <*> Codec.project
                            (\(_, b, _) -> b)
                            (Codec.optional (name "minLength") codec)
                    <*> Codec.project
                            (\(_, _, c) -> c)
                            (Codec.optional (name "maxLength") codec)
            arrayCodec =
                Codec.mapMaybe
                        (\(_, lo, hi, e, m) -> Just $ Schema.Array
                            (Optional.toMaybe lo)
                            (Optional.toMaybe hi)
                            e
                            (Optional.toMaybe m)
                        )
                        (\x -> case x of
                            Schema.Array lo hi e m -> Just
                                ( ()
                                , Optional.fromMaybe lo
                                , Optional.fromMaybe hi
                                , e
                                , Optional.fromMaybe m
                                )
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    $ (,,,,)
                    <$> Codec.project
                            (\(a, _, _, _, _) -> a)
                            (Codec.required (name "type")
                            . Codec.literalCodec
                            . Value.String
                            . String.fromText
                            $ Text.pack "array"
                            )
                    <*> Codec.project
                            (\(_, b, _, _, _) -> b)
                            (Codec.optional (name "minItems") codec)
                    <*> Codec.project
                            (\(_, _, c, _, _) -> c)
                            (Codec.optional (name "maxItems") codec)
                    <*> Codec.project
                            (\(_, _, _, d, _) -> d)
                            (Codec.required (name "items")
                            . Codec.withIdentifier
                                  (Identifier.fromText
                                  $ Text.pack "OneOf Schema [Schema]"
                                  )
                            $ Codec.mapMaybe
                                  (Just . Left)
                                  (either Just $ const Nothing)
                                  codec
                            <|> Codec.mapMaybe
                                    (Just . Right)
                                    (either (const Nothing) Just)
                                    codec
                            )
                    <*> Codec.project
                            (\(_, _, _, _, e) -> e)
                            (Codec.optional (name "additionalItems") codec)
            objectCodec =
                Codec.mapMaybe
                        (\(_, ps, rs, m) -> Just $ Schema.Object
                            (fmap Member.toTuple
                            . foldMap Object.toList
                            $ Optional.toMaybe ps
                            )
                            (Maybe.fromMaybe [] $ Optional.toMaybe rs)
                            (Optional.toMaybe m)
                        )
                        (\x -> case x of
                            Schema.Object ps rs m -> Just
                                ( ()
                                , if null ps
                                    then Optional.nothing
                                    else Optional.just . Object.fromList $ fmap
                                        Member.fromTuple
                                        ps
                                , if null rs
                                    then Optional.nothing
                                    else Optional.just rs
                                , Optional.fromMaybe m
                                )
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    $ (,,,)
                    <$> Codec.project
                            (\(a, _, _, _) -> a)
                            (Codec.required (name "type")
                            . Codec.literalCodec
                            . Value.String
                            . String.fromText
                            $ Text.pack "object"
                            )
                    <*> Codec.project
                            (\(_, b, _, _) -> b)
                            (Codec.optional (name "properties") codec)
                    <*> Codec.project
                            (\(_, _, c, _) -> c)
                            (Codec.optional (name "required") codec)
                    <*> Codec.project
                            (\(_, _, _, d) -> d)
                            (Codec.optional (name "additionalProperties") codec
                            )
            refCodec =
                Codec.mapMaybe
                        (Just . Schema.Ref)
                        (\x -> case x of
                            Schema.Ref y -> Just y
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    $ Codec.required (name "$ref") codec
            oneOfCodec =
                Codec.mapMaybe
                        (Just . Schema.OneOf)
                        (\x -> case x of
                            Schema.OneOf y -> Just y
                            _ -> Nothing
                        )
                    . Codec.fromObjectCodec Permission.Forbid
                    $ Codec.required (name "oneOf") codec
        in
            Codec.identified
            $ trueCodec
            <|> falseCodec
            <|> constCodec
            <|> nullCodec
            <|> booleanCodec
            <|> numberCodec
            <|> integerCodec
            <|> stringCodec
            <|> arrayCodec
            <|> objectCodec
            <|> refCodec
            <|> oneOfCodec

name :: String -> Name.Name
name = Name.fromString . String.fromText . Text.pack

valueCodec
    :: forall a
     . Typeable.Typeable a
    => Schema.Schema
    -> (a -> Value.Value)
    -> (Value.Value -> Maybe a)
    -> Codec.Value a
valueCodec schema toValue fromValue = Codec.identified Codec.Codec
    { Codec.decode = castValue
        (Codec.typeName (Typeable.Proxy :: Typeable.Proxy a))
        fromValue
    , Codec.encode = Codec.tap $ Trans.lift . Trans.put . toValue
    , Codec.schema = pure $ Schema.unidentified schema
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

integralCodec
    :: forall a
     . (Bits.Bits a, Bounded a, Integral a, Typeable.Typeable a)
    => Codec.Value a
integralCodec =
    let
        from = Bits.toIntegralSized :: Integer -> Maybe a
        into = fromIntegral :: a -> Integer
        schema = Schema.unidentified $ Schema.Integer
            (Just $ toInteger (minBound :: a))
            (Just $ toInteger (maxBound :: a))
    in Codec.identified $ Codec.mapMaybe
        from
        (Just . into)
        codec { Codec.schema = pure schema }

optionalNullable
    :: Typeable.Typeable a
    => Name.Name
    -> Codec.Value a
    -> Codec.Object (Maybe a)
optionalNullable k =
    Codec.map
            (maybe Nothing Nullable.toMaybe . Optional.toMaybe)
            (Optional.fromMaybe . fmap Nullable.just)
        . Codec.optional k
        . nullable

nullable
    :: Typeable.Typeable a
    => Codec.Value a
    -> Codec.Value (Nullable.Nullable a)
nullable c =
    Codec.identified
        $ Codec.mapMaybe (Just . Nullable.just) Nullable.toMaybe c
        <|> Codec.map (const Nullable.nothing) (const $ Null.fromUnit ()) codec
