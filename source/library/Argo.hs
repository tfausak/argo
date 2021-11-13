{-# LANGUAGE PatternSynonyms #-}

module Argo
    ( Value.Value
        ( Pattern.Null
        , Pattern.Boolean
        , Pattern.Number
        , Pattern.String
        , Pattern.Array
        , Pattern.Object
        )
    , Name.Name(Pattern.Name)
    , Member.MemberOf(Member)
    , Encode.encode
    , Encode.encodeWith
    , Indent.Indent(Spaces, Tab)
    , Decode.decode
    , FromValue.fromValue
    , ToValue.toValue
    , HasCodec.HasCodec(codec)
    , QuasiQuoter.value
    , QuasiQuoter.pointer
    , Pointer.Pointer(Pointer)
    , Token.Token(Token)
    , Pointer.evaluate
    , Encode.encodePointer
    , Decode.decodePointer
    , Decimal.Decimal(Pattern.Decimal)
    ) where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Class.HasCodec as HasCodec
import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Decode as Decode
import qualified Argo.Encode as Encode
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Value as Value
import qualified Argo.Pattern as Pattern
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Pointer.Token as Token
import qualified Argo.QuasiQuoter as QuasiQuoter
import qualified Argo.Type.Decimal as Decimal
import qualified Argo.Type.Indent as Indent
