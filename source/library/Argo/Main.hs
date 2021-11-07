module Argo.Main where

import qualified Argo
import qualified Argo.Type.Flag as Flag
import qualified Argo.Type.Indent as Indent
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Version as Version
import qualified Paths_argo as This
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

main :: IO ()
main = do
    name <- Environment.getProgName
    arguments <- Environment.getArgs
    let (flags, as, os, es) = Console.getOpt' Console.Permute options arguments
    mapM_ (IO.hPutStrLn IO.stderr . mappend "unknown argument " . quote) as
    mapM_ (IO.hPutStrLn IO.stderr . mappend "unknown option " . quote) os
    mapM_ (IO.hPutStr IO.stderr) es
    Monad.unless (null es) Exit.exitFailure

    settings <- either fail pure $ Monad.foldM applyFlag defaultSettings flags
    Monad.when (settingsHelp settings) $ do
        putStr $ Console.usageInfo (name <> " version " <> version) options
        Exit.exitSuccess
    Monad.when (settingsVersion settings) $ do
        putStrLn version
        Exit.exitSuccess

    contents <- ByteString.getContents
    case Argo.decode contents of
        Argo.Failure e -> fail e
        Argo.Success value -> Builder.hPutBuilder IO.stdout
            $ Argo.encodeWith (settingsIndent settings) (value :: Argo.Value)

quote :: String -> String
quote x = "`" <> x <> "'"

version :: String
version = Version.showVersion This.version

options :: [Console.OptDescr Flag.Flag]
options =
    [ Console.Option ['h', '?'] ["help"] (Console.NoArg Flag.Help) "shows this help message and exits"
    , Console.Option ['v'] ["version"] (Console.NoArg Flag.Version) "shows the version number and exits"
    , Console.Option ['s'] ["spaces"] (Console.ReqArg Flag.Spaces "INT") "pretty-prints the output using INT sapces"
    , Console.Option ['t'] ["tab"] (Console.NoArg Flag.Tab) "pretty-prints the output using tabs"
    ]

data Settings = Settings
    { settingsHelp :: Bool
    , settingsIndent :: Indent.Indent
    , settingsVersion :: Bool
    } deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings
    { settingsHelp = False
    , settingsIndent = Indent.Spaces 0
    , settingsVersion = False
    }

applyFlag :: Settings -> Flag.Flag -> Either String Settings
applyFlag settings flag = case flag of
    Flag.Help -> pure settings { settingsHelp = True }
    Flag.Spaces string -> do
        int <- Read.readEither string
        pure settings { settingsIndent = Indent.Spaces int }
    Flag.Tab -> pure settings { settingsIndent = Indent.Tab }
    Flag.Version -> pure settings { settingsVersion = True }
