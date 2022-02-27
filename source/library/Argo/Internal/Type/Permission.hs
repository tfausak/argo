{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Type.Permission where

import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH

data Permission
    = Allow
    | Forbid
    deriving (Eq, TH.Lift, Show)

instance DeepSeq.NFData Permission where
    rnf = flip seq ()

toBool :: Permission -> Bool
toBool x = case x of
    Allow -> True
    Forbid -> False
