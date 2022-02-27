module Argo.Internal.Decode where

import qualified Argo.Internal.Class.HasCodec as HasCodec
import qualified Argo.Internal.Codec.Value as Codec
import qualified Argo.Internal.Json.Value as Value
import qualified Argo.Internal.Pointer.Pointer as Pointer
import qualified Argo.Internal.Type.Decoder as Decoder
import qualified Argo.Vendor.ByteString as ByteString

decode :: HasCodec.HasCodec a => ByteString.ByteString -> Either String a
decode x = do
    y <- Decoder.run (Decoder.spaces *> Value.decode) x
    fromValue y

decodePointer :: ByteString.ByteString -> Either String Pointer.Pointer
decodePointer = Decoder.run Pointer.decode

fromValue :: HasCodec.HasCodec a => Value.Value -> Either String a
fromValue = Codec.decodeWith HasCodec.codec
