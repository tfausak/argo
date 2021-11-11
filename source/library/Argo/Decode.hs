module Argo.Decode where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Json.Value as Value
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Vendor.ByteString as ByteString

decode :: FromValue.FromValue a => ByteString.ByteString -> Either String a
decode x = do
    y <- Decoder.run (Decoder.spaces *> Value.decode) x
    FromValue.fromValue y

decodePointer :: ByteString.ByteString -> Either String Pointer.Pointer
decodePointer = Decoder.run Pointer.decode
