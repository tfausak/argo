module Argo.Type.Number where

import qualified Control.DeepSeq as DeepSeq

data Number
    = Number Integer Integer
    deriving (Eq, Show)

instance DeepSeq.NFData Number where
    rnf (Number x y) = DeepSeq.deepseq x $ DeepSeq.rnf y
