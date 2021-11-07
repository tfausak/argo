module Argo.Encode where

import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Encoder as Encoder
import qualified Argo.Json.Value as Value
import qualified Argo.Literal as Literal
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Type.Indent as Indent
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad

encode :: ToValue.ToValue a => a -> Builder.Builder
encode = encodeWith $ Indent.Spaces 0

encodeWith :: ToValue.ToValue a => Indent.Indent -> a -> Builder.Builder
encodeWith i x =
    let c = Encoder.defaultConfig { Encoder.indent = i }
    in snd . Encoder.run c $ do
        Value.encode $ ToValue.toValue x
        Monad.when (Encoder.hasIndent c)
            . Trans.lift
            . Trans.tell
            $ Builder.word8 Literal.newLine

encodePointer :: Pointer.Pointer -> Builder.Builder
encodePointer =
    let c = Encoder.defaultConfig
    in snd . Encoder.run c . Pointer.encode
