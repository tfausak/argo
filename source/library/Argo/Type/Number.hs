module Argo.Type.Number where

import qualified Argo.Literal as Literal
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Builder as Builder

data Number
    = Number Integer Integer
    deriving (Eq, Show)

instance DeepSeq.NFData Number where
    rnf (Number x y) = DeepSeq.deepseq x $ DeepSeq.rnf y

normalize :: Number -> Number
normalize (Number x y) =
    if x == 0
    then Number 0 0
    else let (q, r) = quotRem x 10 in if r == 0
    then normalize $ Number q (y + 1)
    else Number x y

encode :: Number -> Builder.Builder
encode (Number x y) =
    if y == 0
    then Builder.integerDec x
    else Builder.integerDec x
        <> Builder.word8 Literal.latinSmallLetterE
        <> Builder.integerDec y
