module Argo.Internal.Type.Settings where

import qualified Argo.Internal.Type.Flag as Flag
import qualified Argo.Internal.Type.Indent as Indent
import qualified Text.Read as Read

data Settings = Settings
    { help :: Bool
    , indent :: Indent.Indent
    , version :: Bool
    }
    deriving (Eq, Show)

initial :: Settings
initial = Settings { help = False, indent = Indent.Spaces 0, version = False }

applyFlag :: Settings -> Flag.Flag -> Either String Settings
applyFlag settings flag = case flag of
    Flag.Help -> pure settings { help = True }
    Flag.Spaces string -> do
        int <- Read.readEither string
        pure settings { indent = Indent.Spaces int }
    Flag.Tab -> pure settings { indent = Indent.Tab }
    Flag.Version -> pure settings { version = True }
