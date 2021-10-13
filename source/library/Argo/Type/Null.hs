module Argo.Type.Null where

import qualified Argo.Literal as Literal
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder

newtype Null
    = Null ()
    deriving (Eq, Show)

instance DeepSeq.NFData Null where
    rnf (Null x) = DeepSeq.rnf x

encode :: Null -> Builder.Builder
encode = const $ Builder.byteString Literal.null
