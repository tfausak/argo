{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Type.Permission where

import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified GHC.Generics as Generics

data Permission
    = Allow
    | Forbid
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

toBool :: Permission -> Bool
toBool x = case x of
    Allow -> True
    Forbid -> False
