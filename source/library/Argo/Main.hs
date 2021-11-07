module Argo.Main where

import qualified Argo
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

data Flag
    = FlagHelp
    | FlagSpaces String
    | FlagTab
    | FlagVersion
    deriving (Eq, Show)

options :: [Console.OptDescr Flag]
options =
    [ Console.Option ['h', '?'] ["help"] (Console.NoArg FlagHelp) "shows this help message and exits"
    , Console.Option ['v'] ["version"] (Console.NoArg FlagVersion) "shows the version number and exits"
    , Console.Option ['s'] ["spaces"] (Console.ReqArg FlagSpaces "INT") "pretty-prints the output using INT sapces"
    , Console.Option ['t'] ["tab"] (Console.NoArg FlagTab) "pretty-prints the output using tabs"
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

applyFlag :: Config -> Flag -> Either String Config
applyFlag config flag = case flag of
    FlagHelp -> pure config { configHelp = True }
    FlagSpaces string -> do
        int <- Read.readEither string
        pure config { configIndent = Indent.Spaces int }
    FlagTab -> pure config { configIndent = Indent.Tab }
    FlagVersion -> pure config { configVersion = True }
