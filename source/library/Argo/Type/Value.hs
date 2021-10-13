module Argo.Type.Value where

import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.String as String
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder

data Value
    = Null Null.Null
    | Boolean Boolean.Boolean
    | Number Number.Number
    | String String.String
    | Array (Array.Array Value)
    | Object (Object.Object Value)
    deriving (Eq, Show)

instance DeepSeq.NFData Value where
    rnf x = case x of
        Null y -> DeepSeq.rnf y
        Boolean y -> DeepSeq.rnf y
        Number y -> DeepSeq.rnf y
        String y -> DeepSeq.rnf y
        Array y -> DeepSeq.rnf y
        Object y -> DeepSeq.rnf y

encode :: Value -> Builder.Builder
encode x = case x of
    Null y -> Null.encode y
    Boolean y -> Boolean.encode y
    Number y -> Number.encode y
    String y -> String.encode y
    Array y -> Array.encode encode y
    Object y -> Object.encode encode y
