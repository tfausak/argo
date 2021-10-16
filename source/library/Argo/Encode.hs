module Argo.Encode where

import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Type.Value as Value
import qualified Data.ByteString.Builder as Builder

encode :: ToValue.ToValue a => a -> Builder.Builder
encode = encodeWith ToValue.toValue

encodeWith :: (a -> Value.Value) -> a -> Builder.Builder
encodeWith f = Value.encode . f
