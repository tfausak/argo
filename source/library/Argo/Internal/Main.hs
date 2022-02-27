module Argo.Internal.Main where

import qualified Argo.Internal.Decode as Decode
import qualified Argo.Internal.Encode as Encode
import qualified Argo.Internal.Json.Value as Value
import qualified Argo.Internal.Type.Flag as Flag
import qualified Argo.Internal.Type.Settings as Settings
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.ByteString as ByteString
import qualified Control.Monad as Monad
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
    let ((warnings, errors), flags) = getFlags arguments
    mapM_ (IO.hPutStrLn IO.stderr) warnings
    mapM_ (IO.hPutStr IO.stderr) errors
    Monad.unless (null errors) Exit.exitFailure

    withSettings name flags $ \settings -> do
        contents <- ByteString.getContents
        value <- case Decode.decode contents of
            Left e -> fail e
            Right x -> pure (x :: Value.Value)
        Builder.hPutBuilder IO.stdout
            $ Encode.encodeWith (Settings.indent settings) value

getFlags :: [String] -> (([String], [String]), [Flag.Flag])
getFlags arguments =
    let
        (flags, xs, ys, errors) =
            Console.getOpt' Console.Permute options arguments
        warnings =
            fmap (mappend "unknown argument " . quote) xs
                <> fmap (mappend "unknown option " . quote) ys
    in ((warnings, errors), flags)

withSettings :: String -> [Flag.Flag] -> (Settings.Settings -> IO ()) -> IO ()
withSettings name flags callback = do
    settings <- either fail pure
        $ Monad.foldM Settings.applyFlag Settings.initial flags
    if Settings.help settings
        then putStr
            $ Console.usageInfo (name <> " version " <> version) options
        else if Settings.version settings
            then putStrLn version
            else callback settings

quote :: String -> String
quote x = "`" <> x <> "'"

version :: String
version = Version.showVersion This.version

options :: [Console.OptDescr Flag.Flag]
options =
    [ Console.Option
        ['h', '?']
        ["help"]
        (Console.NoArg Flag.Help)
        "shows this help message and exits"
    , Console.Option
        ['v']
        ["version"]
        (Console.NoArg Flag.Version)
        "shows the version number and exits"
    , Console.Option
        ['s']
        ["spaces"]
        (Console.ReqArg Flag.Spaces "INT")
        "pretty-prints the output using INT sapces"
    , Console.Option
        ['t']
        ["tab"]
        (Console.NoArg Flag.Tab)
        "pretty-prints the output using tabs"
    ]
