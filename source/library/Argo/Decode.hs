module Argo.Decode where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Decoder as Decoder
import qualified Argo.Result as Result
import qualified Argo.Type.Value as Value
import qualified Argo.Vendor.ByteString as ByteString

decode :: FromValue.FromValue a => ByteString.ByteString -> Result.Result a
decode x =
    case Decoder.run (Decoder.spaces *> Value.decode <* Decoder.eof) x of
        Result.Failure e -> Result.Failure e
        Result.Success (_, y) -> FromValue.fromValue y
