module Argo.Type.Value
    ( Value(..)
    ) where

import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.String as String
import qualified Control.DeepSeq as DeepSeq

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
