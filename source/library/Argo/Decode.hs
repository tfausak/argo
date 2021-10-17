module Argo.Decode where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Decoder as Decoder
import qualified Argo.Type.Value as Value
import qualified Data.ByteString as ByteString

decode :: FromValue.FromValue a => ByteString.ByteString -> Maybe a
decode = decodeWith FromValue.fromValue

decodeWith :: (Value.Value -> Maybe a) -> ByteString.ByteString -> Maybe a
decodeWith f x = do
    (_, y) <- Decoder.run (Decoder.spaces *> Value.decode <* Decoder.eof) x
    f y
