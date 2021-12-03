module Argo.Class.ToValue where

import qualified Argo.Class.HasCodec as HasCodec
import qualified Argo.Codec.Codec as Codec
import qualified Argo.Json.Value as Value

toValue :: HasCodec.HasCodec a => a -> Value.Value
toValue = Codec.encodeWith HasCodec.codec
