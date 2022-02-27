{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Schema.Schema where

import qualified Argo.Internal.Json.Member as Member
import qualified Argo.Internal.Json.Name as Name
import qualified Argo.Internal.Json.String as String
import qualified Argo.Internal.Json.Value as Value
import qualified Argo.Internal.Schema.Identifier as Identifier
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text
import qualified Data.List.NonEmpty as NonEmpty
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
    | Object [(Name.Name, Schema)] [Name.Name] (Maybe Schema)
    | OneOf [Schema]
    | Ref Identifier.Identifier
    | String (Maybe Natural.Natural) (Maybe Natural.Natural)
    | True
    deriving (Eq, TH.Lift, Show)

instance DeepSeq.NFData Schema where
    rnf x = case x of
        Array a b c d -> DeepSeq.rnf (a, b, c, d)
        Boolean -> ()
        Const a -> DeepSeq.rnf a
        Argo.Internal.Schema.Schema.False -> ()
        Integer a b -> DeepSeq.rnf (a, b)
        Null -> ()
        Number -> ()
        Object a b c -> DeepSeq.rnf (a, b, c)
        OneOf a -> DeepSeq.rnf a
        Ref a -> DeepSeq.rnf a
        String a b -> DeepSeq.rnf (a, b)
        Argo.Internal.Schema.Schema.True -> ()

instance Semigroup Schema where
    x <> y = case (x, y) of
        (OneOf xs, OneOf ys) -> OneOf $ xs <> ys
        (OneOf xs, _) -> OneOf $ y : xs
        (_, OneOf ys) -> OneOf $ x : ys
        (_, _) -> OneOf [x, y]

instance Monoid Schema where
    mempty = Argo.Internal.Schema.Schema.True

member :: String -> a -> Member.Member a
member k v =
    Member.fromTuple (Name.fromString . String.fromText $ Text.pack k, v)

maybeRef :: (Maybe Identifier.Identifier, Schema) -> Schema
maybeRef (m, s) = maybe s Ref m

false :: Schema
false = Argo.Internal.Schema.Schema.False

true :: Schema
true = Argo.Internal.Schema.Schema.True

unidentified :: Schema -> (Maybe Identifier.Identifier, Schema)
unidentified s = (Nothing, s)

withIdentifier
    :: Identifier.Identifier -> Schema -> (Maybe Identifier.Identifier, Schema)
withIdentifier i s = (Just i, s)
