module Argo.Type where

import qualified Argo.Type.Member as Member
import qualified Argo.Type.Value as Value
import qualified Argo.Vendor.Array as Array

type Array = Array.Array Int Value.Value

type Member = Member.Member Value.Value

type Object = Array.Array Int Member
