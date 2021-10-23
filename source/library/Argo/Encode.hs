module Argo.Encode where

import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Encoder as Encoder
import qualified Argo.Literal as Literal
import qualified Argo.Json.Value as Value
import qualified Argo.Vendor.Builder as Builder

encode :: ToValue.ToValue a => a -> Builder.Builder
encode = (<> Builder.word8 Literal.newLine) . Encoder.run Value.encode 0 . ToValue.toValue
