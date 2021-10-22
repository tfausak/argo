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
    , Type.Array
    , Name.Name(Pattern.Name)
    , Member.Member(Pattern.Member)
    , Type.Object
    , Encode.encode
    , Decode.decode
    , FromValue.FromValue(fromValue)
    , ToValue.ToValue(toValue)
    , QuasiQuoter.value
    , Result.Result(Failure, Success)
    ) where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Decode as Decode
import qualified Argo.Encode as Encode
import qualified Argo.Pattern as Pattern
import qualified Argo.QuasiQuoter as QuasiQuoter
import qualified Argo.Result as Result
import qualified Argo.Type as Type
import qualified Argo.Type.Member as Member
import qualified Argo.Type.Name as Name
import qualified Argo.Type.Value as Value
