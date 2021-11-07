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
    , Pattern.Array
    , Array.ArrayOf
    , Name.Name
    , pattern Pattern.Name
    , Pattern.Member
    , Member.MemberOf(Member)
    , Pattern.Object
    , Object.ObjectOf
    , Encode.encode
    , Encode.encodeWith
    , Indent.Indent(Spaces, Tab)
    , Decode.decode
    , FromValue.FromValue(fromValue)
    , ToValue.ToValue(toValue)
    , QuasiQuoter.value
    , QuasiQuoter.pointer
    , Result.Result(Failure, Success)
    , Pointer.Pointer(Pointer)
    , Token.Token(Token)
    , Pointer.evaluate
    , Encode.encodePointer
    , Decode.decodePointer
    ) where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Decode as Decode
import qualified Argo.Encode as Encode
import qualified Argo.Json.Array as Array
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Object as Object
import qualified Argo.Json.Value as Value
import qualified Argo.Pattern as Pattern
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Pointer.Token as Token
import qualified Argo.QuasiQuoter as QuasiQuoter
import qualified Argo.Type.Indent as Indent
import qualified Argo.Type.Result as Result
