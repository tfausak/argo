module Argo.Decode where

import qualified Argo.Class.HasCodec as HasCodec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Json.Value as Value
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Vendor.ByteString as ByteString

decode :: HasCodec.HasCodec a => ByteString.ByteString -> Either String a
decode x = do
    y <- Decoder.run (Decoder.spaces *> Value.decode) x
    Codec.decodeWith HasCodec.codec y

decodePointer :: ByteString.ByteString -> Either String Pointer.Pointer
decodePointer = Decoder.run Pointer.decode
