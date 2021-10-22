module Argo.QuasiQuoter where

import qualified Argo.Decode as Decode
import qualified Argo.Result as Result
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

value :: TH.QuasiQuoter
value = TH.QuasiQuoter
    { TH.quoteDec = const $ fail "quoteDec"
    , TH.quoteExp = quoteExp
    , TH.quotePat = const $ fail "quotePat"
    , TH.quoteType = const $ fail "quoteType"
    }

quoteExp :: String -> TH.Q TH.Exp
quoteExp x = case Decode.decodeWith pure . Text.encodeUtf8 $ Text.pack x of
    Result.Failure e -> fail e
    Result.Success y -> TH.lift y
