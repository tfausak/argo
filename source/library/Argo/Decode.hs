module Argo.Decode where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Decoder as Decoder
import qualified Argo.Result as Result
import qualified Argo.Type.Value as Value
import qualified Data.ByteString as ByteString

decode :: FromValue.FromValue a => ByteString.ByteString -> Result.Result a
decode = decodeWith FromValue.fromValue

decodeWith :: (Value.Value -> Result.Result a) -> ByteString.ByteString -> Result.Result a
decodeWith f x =
    case Decoder.run (Decoder.spaces *> Value.decode <* Decoder.eof) x of
        Result.Failure e -> Result.Failure e
        Result.Success (_, y) -> f y
