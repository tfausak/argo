module Argo.Encode where

import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Json.Value as Value
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.Functor.Identity as Identity

encode :: ToValue.ToValue a => a -> Builder.Builder
encode x = Identity.runIdentity
    . Trans.execWriterT
    . flip Trans.runReaderT ()
    $ do
        Value.encode $ ToValue.toValue x
