module Argo.Decode where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Decoder as Decoder
import qualified Argo.Json.Value as Value
import qualified Argo.Pointer.Pointer as Pointer
import qualified Argo.Result as Result
import qualified Argo.Vendor.ByteString as ByteString

decode :: FromValue.FromValue a => ByteString.ByteString -> Result.Result a
decode x = do
    (_, y) <- Decoder.run (Decoder.spaces *> Value.decode <* Decoder.eof) x
    FromValue.fromValue y

decodePointer :: ByteString.ByteString -> Result.Result Pointer.Pointer
decodePointer =
    let d = Pointer.decode <* Decoder.eof
    in fmap snd . Decoder.run d
