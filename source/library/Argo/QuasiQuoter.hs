module Argo.QuasiQuoter where

import qualified Language.Haskell.TH.Quote as QQ

value :: QQ.QuasiQuoter
value = QQ.QuasiQuoter
    { QQ.quoteDec = const $ fail "quoteDec"
    , QQ.quoteExp = const $ fail "quoteExp"
    , QQ.quotePat = const $ fail "quotePat"
    , QQ.quoteType = const $ fail "quoteType"
    }
