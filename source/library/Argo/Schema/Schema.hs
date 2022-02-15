{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Schema.Schema where

-- import qualified Argo.Json.Array as Array
-- import qualified Argo.Json.Boolean as Boolean
-- import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
-- import qualified Argo.Json.Object as Object
-- import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Schema.Identifier as Identifier
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
-- import qualified Argo.Vendor.Text as Text
import qualified GHC.Generics as Generics

-- | A JSON Schema.
-- <https://datatracker.ietf.org/doc/html/draft-handrews-json-schema-01>
data Schema
    = Array
        Permission.Permission
        [(Maybe Identifier.Identifier, Schema)]
    | Boolean
    | Const Value.Value
    | Integer (Maybe Integer) (Maybe Integer)
    | Null
    | Number
    | Object
        Permission.Permission
        [((Name.Name, Bool), (Maybe Identifier.Identifier, Schema))]
        (Maybe (Maybe Identifier.Identifier, Schema))
    | Ref Identifier.Identifier
    | String (Maybe Integer) (Maybe Integer)
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

instance Semigroup Schema where
    (<>) = error "TODO"

instance Monoid Schema where
    mempty = error "TODO"

toValue :: Schema -> Value.Value
toValue = error "TODO"

false :: Schema
false = error "TODO"

true :: Schema
true = error "TODO"

{- const
Value.Object $ Object.fromList
    [ Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack "const"
        , expected
        )
    ]
-}

{- ref
Value.Object $ Object.fromList
    [ Member.fromTuple
            ( Name.fromString . String.fromText $ Text.pack "$ref"
            , Value.String
            . String.fromText
            . mappend (Text.pack "#/definitions/")
            $ Identifier.toText i
            )
    ]
-}

{- object
Value.Object $ Object.fromList
    [ Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "type"
        , Value.String . String.fromText $ Text.pack
            "object"
        )
    , Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "properties"
        , Value.Object . Object.fromList $ fmap
            (\((k, _), (_, s)) ->
                Member.fromTuple (k, Schema.toValue s)
            )
            schemas
        )
    , Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "required"
        , Value.Array . Array.fromList $ Maybe.mapMaybe
            (\((k, r), _) -> if r
                then Just . Value.String $ Name.toString
                    k
                else Nothing
            )
            schemas
        )
    , Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "additionalProperties"
        , Value.Boolean
        . Boolean.fromBool
        $ case permission of
            Permission.Allow -> True
            Permission.Forbid -> False
        )
    ]
-}

{- array
Value.Object . Object.fromList
    $ member
            "type"
            (Value.String . String.fromText $ Text.pack "array")
    : member
            "minItems"
            (Value.Number
            . Number.fromDecimal
            . Decimal.fromInteger
            . toInteger
            $ length schemas
            )
    : if null schemas
            then
                [ member "maxItems"
                . Value.Number
                . Number.fromDecimal
                $ Decimal.fromInteger 0
                ]
            else
                [ member "items"
                . Value.Array
                . Array.fromList
                $ fmap (Schema.toValue . snd) schemas
                , member "additionalItems"
                . Value.Boolean
                . Boolean.fromBool
                $ Permission.toBool permission
                ]
-}

{- null
Value.Object $ Object.fromList
    [ Member.fromTuple
            ( Name.fromString . String.fromText $ Text.pack "type"
            , Value.String . String.fromText $ Text.pack "null"
            )
    ]
-}

{- boolean
Value.Object $ Object.fromList
    [ Member.fromTuple
            ( Name.fromString . String.fromText $ Text.pack "type"
            , Value.String . String.fromText $ Text.pack "boolean"
            )
    ]
-}

{- string
Value.Object $ Object.fromList
    [ Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "type"
        , Value.String . String.fromText $ Text.pack
            "string"
        )
    , Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "minLength"
        , Value.Number
        . Number.fromDecimal
        $ Decimal.fromInteger 1
        )
    , Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "maxLength"
        , Value.Number
        . Number.fromDecimal
        $ Decimal.fromInteger 1
        )
    ]
-}

{- number
Value.Object $ Object.fromList
    [ Member.fromTuple
            ( Name.fromString . String.fromText $ Text.pack "type"
            , Value.String . String.fromText $ Text.pack "number"
            )
    ]
-}

{- integer
Value.Object $ Object.fromList
    [ Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "type"
        , Value.String . String.fromText $ Text.pack
            "integer"
        )
    , Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "minimum"
        , Value.Number
        . Number.fromDecimal
        . Decimal.fromInteger
        $ toInteger (minBound :: a)
        )
    , Member.fromTuple
        ( Name.fromString . String.fromText $ Text.pack
            "maximum"
        , Value.Number
        . Number.fromDecimal
        . Decimal.fromInteger
        $ toInteger (maxBound :: a)
        )
    ]
-}

unidentified :: Schema -> (Maybe Identifier.Identifier, Schema)
unidentified s = (Nothing, s)

withIdentifier
    :: Identifier.Identifier -> Schema -> (Maybe Identifier.Identifier, Schema)
withIdentifier i s = (Just i, s)
