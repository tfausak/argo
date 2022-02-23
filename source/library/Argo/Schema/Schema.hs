{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Schema.Schema where

import qualified Argo.Json.Array as Array
import qualified Argo.Json.Boolean as Boolean
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Number as Number
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Schema.Identifier as Identifier
import qualified Argo.Type.Decimal as Decimal
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import qualified GHC.Generics as Generics
import qualified Numeric.Natural as Natural

-- | A JSON Schema.
-- <https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-01>
data Schema
    = Array
        (Maybe Natural.Natural)
        (Maybe Natural.Natural)
        (Either Schema (NonEmpty.NonEmpty Schema))
        (Maybe Schema)
    | Boolean
    | Const Value.Value
    | False
    | Integer (Maybe Integer) (Maybe Integer)
    | Null
    | Number
    | Object
        [(Name.Name, Schema)]
        [Name.Name]
        (Maybe Schema)
    | OneOf [Schema]
    | Ref Identifier.Identifier
    | String (Maybe Natural.Natural) (Maybe Natural.Natural)
    | True
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

instance Semigroup Schema where
    x <> y = case (x, y) of
        (OneOf xs, OneOf ys) -> OneOf $ xs <> ys
        (OneOf xs, _) -> OneOf $ y : xs
        (_, OneOf ys) -> OneOf $ x : ys
        (_, _) -> OneOf [x, y]

instance Monoid Schema where
    mempty = Argo.Schema.Schema.True

toValue :: Schema -> Value.Value
toValue schema = case schema of
    Array lo hi e m -> Value.Object . Object.fromList $ Maybe.catMaybes
        [ Just . member "type" . Value.String . String.fromText $ Text.pack
            "array"
        , fmap
            (member "minItems"
            . Value.Number
            . Number.fromDecimal
            . Decimal.fromInteger
            . toInteger
            )
            lo
        , fmap
            (member "maxItems"
            . Value.Number
            . Number.fromDecimal
            . Decimal.fromInteger
            . toInteger
            )
            hi
        , Just . member "items" $ case e of
            Left s -> toValue s
            Right xs ->
                Value.Array . Array.fromList . fmap toValue $ NonEmpty.toList
                    xs
        , fmap (member "additionalItems" . toValue) m
        ]

    Boolean -> Value.Object $ Object.fromList
        [member "type" . Value.String . String.fromText $ Text.pack "boolean"]

    Const x -> Value.Object $ Object.fromList [member "const" x]

    Argo.Schema.Schema.False -> Value.Boolean $ Boolean.fromBool Prelude.False

    Integer lo hi -> Value.Object . Object.fromList $ Maybe.catMaybes
        [ Just . member "type" . Value.String . String.fromText $ Text.pack
            "integer"
        , member "minimum"
        . Value.Number
        . Number.fromDecimal
        . Decimal.fromInteger
        <$> lo
        , member "maximum"
        . Value.Number
        . Number.fromDecimal
        . Decimal.fromInteger
        <$> hi
        ]

    Null -> Value.Object $ Object.fromList
        [member "type" . Value.String . String.fromText $ Text.pack "null"]

    Number -> Value.Object $ Object.fromList
        [member "type" . Value.String . String.fromText $ Text.pack "number"]

    Object ps rs m -> Value.Object . Object.fromList $ Maybe.catMaybes
        [ Just . member "type" . Value.String . String.fromText $ Text.pack
            "object"
        , if null ps
            then Nothing
            else
                Just
                . member "properties"
                . Value.Object
                . Object.fromList
                $ fmap (Member.fromTuple . fmap toValue) ps
        , if null rs
            then Nothing
            else Just . member "required" . Value.Array . Array.fromList $ fmap
                (Value.String . Name.toString)
                rs
        , fmap (member "additionalProperties" . toValue) m
        ]

    OneOf xs -> Value.Object $ Object.fromList
        [member "oneOf" . Value.Array . Array.fromList $ fmap toValue xs]

    Ref x -> Value.Object $ Object.fromList
        [ member "$ref"
          . Value.String
          . String.fromText
          . mappend (Text.pack "#/definitions/")
          $ Identifier.toText x
        ]

    String lo hi -> Value.Object . Object.fromList $ Maybe.catMaybes
        [ Just . member "type" . Value.String . String.fromText $ Text.pack
            "string"
        , member "minLength"
        . Value.Number
        . Number.fromDecimal
        . Decimal.fromInteger
        . toInteger
        <$> lo
        , member "maxLength"
        . Value.Number
        . Number.fromDecimal
        . Decimal.fromInteger
        . toInteger
        <$> hi
        ]

    Argo.Schema.Schema.True -> Value.Boolean $ Boolean.fromBool Prelude.True

member :: String -> a -> Member.Member a
member k v =
    Member.fromTuple (Name.fromString . String.fromText $ Text.pack k, v)

ref :: (Maybe Identifier.Identifier, Schema) -> Schema
ref (m, s) = maybe s Ref m

false :: Schema
false = Argo.Schema.Schema.False

true :: Schema
true = Argo.Schema.Schema.True

unidentified :: Schema -> (Maybe Identifier.Identifier, Schema)
unidentified s = (Nothing, s)

withIdentifier
    :: Identifier.Identifier -> Schema -> (Maybe Identifier.Identifier, Schema)
withIdentifier i s = (Just i, s)
