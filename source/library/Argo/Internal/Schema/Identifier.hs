{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Schema.Identifier where

import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text
import qualified Data.String as String

newtype Identifier
    = Identifier Text.Text
    deriving (Eq, TH.Lift, Ord, Show)

instance DeepSeq.NFData Identifier where
    rnf = DeepSeq.rnf . toText

instance String.IsString Identifier where
    fromString = fromText . String.fromString

instance Semigroup Identifier where
    x <> y = fromText $ toText x <> toText y

fromText :: Text.Text -> Identifier
fromText = Identifier

toText :: Identifier -> Text.Text
toText (Identifier x) = x
