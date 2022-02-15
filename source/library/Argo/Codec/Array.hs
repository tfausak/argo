module Argo.Codec.Array where

import qualified Argo.Codec.Codec as Codec
import qualified Argo.Codec.List as Codec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Json.Array as Array
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Schema.Identifier as Identifier
import qualified Argo.Schema.Schema as Schema
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.Map as Map
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.Functor.Identity as Identity

type Array a
    = Codec.List
          ( Trans.AccumT
                (Map.Map Identifier.Identifier Schema.Schema)
                Identity.Identity
                [(Maybe Identifier.Identifier, Schema.Schema)]
          )
          Value.Value
          a

fromArrayCodec :: Permission.Permission -> Array a -> Codec.Value a
fromArrayCodec =
    Codec.fromListCodec
            (\permission schemasM -> do
                schemas <- schemasM
                pure
                    . Schema.unidentified
                    $ Schema.Array permission schemas
            )
        $ Codec.map Array.toList Array.fromList Codec.arrayCodec

member :: String -> a -> Member.Member a
member k v =
    Member.fromTuple (Name.fromString . String.fromText $ Text.pack k, v)

element :: Codec.Value a -> Array a
element c = Codec.Codec
    { Codec.decode = do
        l <- Trans.get
        case l of
            [] -> Trans.lift $ Trans.throwE "unexpected empty list"
            h : t -> case Codec.decodeWith c h of
                Left y -> Trans.lift $ Trans.throwE y
                Right y -> do
                    Trans.put t
                    pure y
    , Codec.encode = \x -> do
        Trans.tell [Codec.encodeWith c x]
        pure x
    , Codec.schema =
        pure
        . Schema.unidentified
        . either id Schema.Ref
        <$> Codec.getRef c
    }
