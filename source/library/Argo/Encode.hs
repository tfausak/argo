module Argo.Encode where

import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Json.Value as Value
import qualified Argo.Literal as Literal
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Type.Config as Config
import qualified Argo.Type.Encoder as Encoder
import qualified Argo.Type.Indent as Indent
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad

encode :: ToValue.ToValue a => a -> Builder.Builder
encode = encodeWith $ Indent.Spaces 0

encodeWith :: ToValue.ToValue a => Indent.Indent -> a -> Builder.Builder
encodeWith i x =
    let c = Config.initial { Config.indent = i }
    in
        Encoder.run c $ do
            Value.encode $ ToValue.toValue x
            Monad.when (Config.hasIndent c)
                . Trans.lift
                . Trans.tell
                $ Builder.word8 Literal.newLine

encodePointer :: Pointer.Pointer -> Builder.Builder
encodePointer = Encoder.run Config.initial . Pointer.encode
