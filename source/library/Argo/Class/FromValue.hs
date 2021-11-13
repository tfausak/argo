module Argo.Class.FromValue where

import qualified Argo.Class.HasCodec as HasCodec
import qualified Argo.Json.Value as Value
import qualified Argo.Type.Codec as Codec

fromValue :: HasCodec.HasCodec a => Value.Value -> Either String a
fromValue = Codec.decodeWith HasCodec.codec
