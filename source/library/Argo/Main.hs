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

    config <- either fail pure $ Monad.foldM applyFlag defaultConfig flags
    Monad.when (configHelp config) $ do
        putStr $ Console.usageInfo (name <> " version " <> version) options
        Exit.exitSuccess
    Monad.when (configVersion config) $ do
        putStrLn version
        Exit.exitSuccess

    contents <- ByteString.getContents
    case Argo.decode contents of
        Argo.Failure e -> fail e
        Argo.Success value -> Builder.hPutBuilder IO.stdout
            $ Argo.encodeWith (configIndent config) (value :: Argo.Value)

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

data Config = Config
    { configHelp :: Bool
    , configIndent :: Indent.Indent
    , configVersion :: Bool
    } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { configHelp = False
    , configIndent = Indent.Spaces 0
    , configVersion = False
    }

applyFlag :: Config -> Flag.Flag -> Either String Config
applyFlag config flag = case flag of
    Flag.Help -> pure config { configHelp = True }
    Flag.Spaces string -> do
        int <- Read.readEither string
        pure config { configIndent = Indent.Spaces int }
    Flag.Tab -> pure config { configIndent = Indent.Tab }
    Flag.Version -> pure config { configVersion = True }
