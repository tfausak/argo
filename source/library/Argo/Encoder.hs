module Argo.Encoder where

import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Data.Functor.Identity as Identity

type Encoder = Trans.WriterT Builder.Builder Identity.Identity
