module Argo.Encode where

import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Encoder as Encoder
import qualified Argo.Json.Value as Value
import qualified Argo.Literal as Literal
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad

encode :: ToValue.ToValue a => a -> Builder.Builder
encode = encodeWith $ Encoder.Spaces 0

encodeWith :: ToValue.ToValue a => Encoder.Indent -> a -> Builder.Builder
encodeWith i x =
    let c = Encoder.Config { Encoder.indent = i, Encoder.level = 0 }
    in snd . Encoder.run c $ do
        Value.encode $ ToValue.toValue x
        Monad.when (Encoder.hasIndent c)
            . Trans.lift
            . Trans.tell
            $ Builder.word8 Literal.newLine

encodePointer :: Pointer.Pointer -> Builder.Builder
encodePointer =
    let c = Encoder.Config { Encoder.indent = Encoder.Spaces 0, Encoder.level = 0 }
    in snd . Encoder.run c . Pointer.encode
