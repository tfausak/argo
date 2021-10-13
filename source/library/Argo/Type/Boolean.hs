module Argo.Type.Boolean where

import qualified Argo.Literal as Literal
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder

newtype Boolean
    = Boolean Bool
    deriving (Eq, Show)

instance DeepSeq.NFData Boolean where
    rnf (Boolean x) = DeepSeq.rnf x

encode :: Boolean -> Builder.Builder
encode (Boolean x) =
    Builder.byteString $ if x then Literal.true else Literal.false
