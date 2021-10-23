module Argo.Encode where

import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Type.Value as Value
import qualified Argo.Vendor.Builder as Builder

encode :: ToValue.ToValue a => a -> Builder.Builder
encode = Value.encode . ToValue.toValue
