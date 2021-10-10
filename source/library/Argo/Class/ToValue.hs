module Argo.Class.ToValue where

import qualified Argo.Type.Value as Value

class ToValue a where
    toValue :: a -> Value.Value

instance ToValue Value.Value where
    toValue = id
