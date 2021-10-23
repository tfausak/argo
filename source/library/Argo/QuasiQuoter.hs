module Argo.QuasiQuoter where

import qualified Argo.Decode as Decode
import qualified Argo.Json.Value as Value
import qualified Argo.Result as Result
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text

value :: TH.QuasiQuoter
value = TH.QuasiQuoter
    { TH.quoteDec = const $ fail "cannot be used as a declaration"
    , TH.quoteExp = quoteExp
    , TH.quotePat = const $ fail "cannot be used as a pattern"
    , TH.quoteType = const $ fail "cannot be used as a type"
    }

quoteExp :: String -> TH.Q TH.Exp
quoteExp x = case Decode.decode . Text.encodeUtf8 $ Text.pack x of
    Result.Failure e -> fail e
    Result.Success y -> TH.lift (y :: Value.Value)
