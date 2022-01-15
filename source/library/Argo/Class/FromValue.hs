module Argo.Class.FromValue where

import qualified Argo.Class.HasCodec as HasCodec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Json.Value as Value

fromValue :: HasCodec.HasCodec a => Value.Value -> Either String a
fromValue = Codec.decodeWith HasCodec.codec
