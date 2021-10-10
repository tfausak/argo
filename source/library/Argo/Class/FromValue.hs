module Argo.Class.FromValue where

import qualified Argo.Type.Value as Value

class FromValue a where
    fromValue :: Value.Value -> Maybe a

instance FromValue Value.Value where
    fromValue = Just
