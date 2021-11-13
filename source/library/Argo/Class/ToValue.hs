module Argo.Class.ToValue where

import qualified Argo.Class.HasCodec as HasCodec
import qualified Argo.Json.Value as Value
import qualified Argo.Type.Codec as Codec

toValue :: HasCodec.HasCodec a => a -> Value.Value
toValue = Codec.encodeWith HasCodec.codec
