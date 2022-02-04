module Argo.Vendor.Text
    ( LazyText
    , Text.Text
    , Text.concatMap
    , Encoding.decodeUtf8'
    , Encoding.encodeUtf8
    , Encoding.encodeUtf8BuilderEscaped
    , LazyText.fromStrict
    , Text.null
    , Text.pack
    , Text.singleton
    , LazyText.toStrict
    , Text.uncons
    , Text.unpack
    ) where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Lazy as LazyText

type LazyText = LazyText.Text
