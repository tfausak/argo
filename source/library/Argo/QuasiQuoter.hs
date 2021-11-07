module Argo.QuasiQuoter where

import qualified Argo.Decode as Decode
import qualified Argo.Json.Value as Value
import qualified Argo.Result as Result
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text

pointer :: TH.QuasiQuoter
pointer = TH.QuasiQuoter
    { TH.quoteDec = const $ fail "cannot be used as a declaration"
    , TH.quoteExp = result fail TH.lift . Decode.decodePointer . Text.encodeUtf8 . Text.pack
    , TH.quotePat = const $ fail "cannot be used as a pattern"
    , TH.quoteType = const $ fail "cannot be used as a type"
    }

value :: TH.QuasiQuoter
value = TH.QuasiQuoter
    { TH.quoteDec = const $ fail "cannot be used as a declaration"
    , TH.quoteExp = result fail TH.lift . fmap asValue . Decode.decode . Text.encodeUtf8 . Text.pack
    , TH.quotePat = const $ fail "cannot be used as a pattern"
    , TH.quoteType = const $ fail "cannot be used as a type"
    }

asValue :: Value.Value -> Value.Value
asValue = id

result :: (String -> b) -> (a -> b) -> Result.Result a -> b
result f g x = case x of
    Result.Failure y -> f y
    Result.Success y -> g y
