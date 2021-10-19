module Argo.QuasiQuoter where

import qualified Argo.Decode as Decode
import qualified Argo.Result as Result
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Language.Haskell.TH.Quote as QQ
import qualified Language.Haskell.TH.Syntax as TH

value :: QQ.QuasiQuoter
value = QQ.QuasiQuoter
    { QQ.quoteDec = const $ fail "quoteDec"
    , QQ.quoteExp = quoteExp
    , QQ.quotePat = const $ fail "quotePat"
    , QQ.quoteType = const $ fail "quoteType"
    }

quoteExp :: String -> TH.Q TH.Exp
quoteExp x = case Decode.decodeWith pure . Text.encodeUtf8 $ Text.pack x of
    Result.Failure e -> fail e
    Result.Success y -> TH.lift y
