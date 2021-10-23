module Argo.Type where

import qualified Argo.Type.Member as Member
import qualified Argo.Type.Value as Value

type Array = [Value.Value]

type Member = Member.MemberOf Value.Value

type Object = [Member]
