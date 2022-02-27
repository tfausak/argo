module Argo.Internal.Encode where

import qualified Argo.Internal.Class.HasCodec as HasCodec
import qualified Argo.Internal.Codec.Value as Codec
import qualified Argo.Internal.Json.Value as Value
import qualified Argo.Internal.Literal as Literal
import qualified Argo.Internal.Pointer.Pointer as Pointer
import qualified Argo.Internal.Type.Config as Config
import qualified Argo.Internal.Type.Encoder as Encoder
import qualified Argo.Internal.Type.Indent as Indent
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad

encode :: HasCodec.HasCodec a => a -> Builder.Builder
encode = encodeWith $ Indent.Spaces 0

encodeWith :: HasCodec.HasCodec a => Indent.Indent -> a -> Builder.Builder
encodeWith i x =
    let c = Config.initial { Config.indent = i }
    in
        Encoder.run c $ do
            Value.encode $ toValue x
            Monad.when (Config.hasIndent c)
                . Trans.lift
                . Trans.tell
                $ Builder.word8 Literal.newLine

encodePointer :: Pointer.Pointer -> Builder.Builder
encodePointer = Encoder.run Config.initial . Pointer.encode

toValue :: HasCodec.HasCodec a => a -> Value.Value
toValue = Codec.encodeWith HasCodec.codec
