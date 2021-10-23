module Argo.Encoder where

import qualified Argo.Vendor.Builder as Builder

newtype Encoder a = Encoder
    { run :: Int -> a -> Builder.Builder
    }
