module Argo.Main where

import qualified Argo
import qualified Argo.Type.Flag as Flag
import qualified Argo.Type.Settings as Settings
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Version as Version
import qualified Paths_argo as This
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

main :: IO ()
main = do
    name <- Environment.getProgName
    arguments <- Environment.getArgs
    mainWith name arguments

mainWith :: String -> [String] -> IO ()
mainWith name arguments = do
    let (flags, as, os, es) = Console.getOpt' Console.Permute options arguments
    mapM_ (IO.hPutStrLn IO.stderr . mappend "unknown argument " . quote) as
    mapM_ (IO.hPutStrLn IO.stderr . mappend "unknown option " . quote) os
    mapM_ (IO.hPutStr IO.stderr) es
    Monad.unless (null es) Exit.exitFailure

    settings <- either fail pure $ Monad.foldM Settings.applyFlag Settings.initial flags
    Monad.when (Settings.help settings) $ do
        putStr $ Console.usageInfo (name <> " version " <> version) options
        Exit.exitSuccess
    Monad.when (Settings.version settings) $ do
        putStrLn version
        Exit.exitSuccess

    contents <- ByteString.getContents
    case Argo.decode contents of
        Argo.Failure e -> fail e
        Argo.Success value -> Builder.hPutBuilder IO.stdout
            $ Argo.encodeWith (Settings.indent settings) (value :: Argo.Value)

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
