module Argo.QuasiQuoter where

import qualified Argo.Decode as Decode
import qualified Argo.Json.Value as Value
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text

pointer :: TH.QuasiQuoter
pointer = defaultQuasiQuoter
    { TH.quoteExp =
        either fail TH.lift
        . Decode.decodePointer
        . Text.encodeUtf8
        . Text.pack
    }

value :: TH.QuasiQuoter
value = defaultQuasiQuoter
    { TH.quoteExp =
        either fail (TH.lift . asValue)
        . Decode.decode
        . Text.encodeUtf8
        . Text.pack
    }

asValue :: Value.Value -> Value.Value
asValue = id

defaultQuasiQuoter :: TH.QuasiQuoter
defaultQuasiQuoter = TH.QuasiQuoter
    { TH.quoteDec = const $ fail "cannot be used as a declaration"
    , TH.quoteExp = const $ fail "cannot be used as an expression"
    , TH.quotePat = const $ fail "cannot be used as a pattern"
    , TH.quoteType = const $ fail "cannot be used as a type"
    }
