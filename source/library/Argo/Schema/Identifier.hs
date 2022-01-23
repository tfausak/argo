{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Schema.Identifier where

import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text
import qualified GHC.Generics as Generics

newtype Identifier
    = Identifier Text.Text
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

instance Semigroup Identifier where
    x <> y = fromText $ toText x <> toText y -- TODO: remove this

fromText :: Text.Text -> Identifier
fromText = Identifier

toText :: Identifier -> Text.Text
toText (Identifier x) = x
