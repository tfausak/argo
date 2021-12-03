module Argo.Codec.Simple where

import qualified Argo.Codec.Codec as Codec

type Simple r w a = Codec.Codec r w a a
