module Argo.Vendor.Builder
    ( Prim.BoundedPrim
    , Builder.Builder
    , Builder.byteString
    , Prim.condB
    , Builder.integerDec
    , Prim.liftFixedToBounded
    , Builder.toLazyByteString
    , Builder.word8
    , word8F
    , Prim.word16HexFixed
    , (Prim.>$<)
    , (Prim.>*<)
    ) where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.Word as Word

word8F :: Prim.FixedPrim Word.Word8
word8F = Prim.word8
