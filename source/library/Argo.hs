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
    , Name.Name
    , pattern Pattern.Name
    , Pattern.Member
    , pattern Pattern.Member
    , Pattern.Object
    , Encode.encode
    , Encode.encodeWith
    , Encoder.Indent(Spaces, Tab)
    , Decode.decode
    , FromValue.FromValue(fromValue)
    , ToValue.ToValue(toValue)
    , QuasiQuoter.value
    , QuasiQuoter.pointer
    , Result.Result(Failure, Success)
    , Pointer.Pointer(Pointer.Pointer)
    , Token.Token(Token.Token)
    , Pointer.evaluate
    , Encode.encodePointer
    , Decode.decodePointer
    ) where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Decode as Decode
import qualified Argo.Encode as Encode
import qualified Argo.Encoder as Encoder
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Value as Value
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Pointer.Token as Token
import qualified Argo.Pattern as Pattern
import qualified Argo.QuasiQuoter as QuasiQuoter
import qualified Argo.Result as Result
