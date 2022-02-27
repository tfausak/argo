module Argo
    ( Value.Value
        ( Pattern.Array
        , Pattern.Boolean
        , Pattern.Null
        , Pattern.Number
        , Pattern.Object
        , Pattern.String
        )
    -- * Decoding
    , Decode.decode
    , Decode.decodePointer
    , Decode.fromValue
    -- * Encoding
    , Encode.encode
    , Encode.encodeWith
    , Encode.encodePointer
    , Encode.toValue
    -- * Codecs
    , HasCodec.HasCodec(codec)
    , Codec.identified
    , Codec.withIdentifier
    , Codec.map
    , Codec.mapMaybe
    , Codec.project
    -- ** Arrays
    , Codec.fromArrayCodec
    , element
    -- ** Objects
    , Codec.fromObjectCodec
    , required
    , optional
    -- * Schemas
    , Codec.schema
    , oneOf
    -- * Pointers
    , Pointer.evaluate
    -- * Quasi Quoters
    , QuasiQuoter.value
    , QuasiQuoter.pointer
    -- * Types
    , Codec
    , Decimal.Decimal(Pattern.Decimal)
    , Identifier.Identifier(Identifier.Identifier)
    , Indent.Indent(Indent.Spaces, Indent.Tab)
    , Member.Member(Member.Member)
    , Name.Name(Pattern.Name)
    , Permission.Permission(Permission.Allow, Permission.Forbid)
    , Pointer.Pointer(Pointer.Pointer)
    , Schema.Schema
    , Token.Token(Token.Token)
    ) where

import qualified Argo.Internal.Class.HasCodec as HasCodec
import qualified Argo.Internal.Codec.Array as Codec
import qualified Argo.Internal.Codec.Codec as Codec
import qualified Argo.Internal.Codec.Object as Codec
import qualified Argo.Internal.Codec.Value as Codec
import qualified Argo.Internal.Decode as Decode
import qualified Argo.Internal.Encode as Encode
import qualified Argo.Internal.Json.Member as Member
import qualified Argo.Internal.Json.Name as Name
import qualified Argo.Internal.Json.Value as Value
import qualified Argo.Internal.Pattern as Pattern
import qualified Argo.Internal.Pointer.Pointer as Pointer
import qualified Argo.Internal.Pointer.Token as Token
import qualified Argo.Internal.QuasiQuoter as QuasiQuoter
import qualified Argo.Internal.Schema.Identifier as Identifier
import qualified Argo.Internal.Schema.Schema as Schema
import qualified Argo.Internal.Type.Decimal as Decimal
import qualified Argo.Internal.Type.Indent as Indent
import qualified Argo.Internal.Type.Permission as Permission
import qualified Data.Foldable as Foldable

type Codec a = Codec.Value a

element :: HasCodec.HasCodec o => (i -> o) -> Codec.Element i o
element f = Codec.project f $ Codec.element HasCodec.codec

oneOf :: Foldable t => t Schema.Schema -> Schema.Schema
oneOf = Foldable.fold

optional
    :: HasCodec.HasCodec f
    => (i -> Maybe f)
    -> Name.Name
    -> Codec.Member i (Maybe f)
optional f k = Codec.project f $ HasCodec.optionalNullable k HasCodec.codec

required :: HasCodec.HasCodec f => (i -> f) -> Name.Name -> Codec.Member i f
required f k = Codec.project f $ Codec.required k HasCodec.codec
