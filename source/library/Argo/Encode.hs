module Argo.Encode where

import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Json.Value as Value
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans

encode :: ToValue.ToValue a => a -> Builder.Builder
encode x = Trans.execWriter $ do
    Value.encode $ ToValue.toValue x
