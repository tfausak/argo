module Argo.Encode where

import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Encoder as Encoder
import qualified Argo.Json.Value as Value
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.Functor.Identity as Identity

encode :: ToValue.ToValue a => a -> Builder.Builder
encode = encodeWith Encoder.Config
    { Encoder.indent = Encoder.Spaces 0
    , Encoder.level = 0
    }

encodeWith :: ToValue.ToValue a => Encoder.Config -> a -> Builder.Builder
encodeWith c x = Identity.runIdentity
    . Trans.execWriterT
    . flip Trans.runReaderT c
    $ do
        Value.encode $ ToValue.toValue x
