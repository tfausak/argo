import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import qualified System.Process as Process

main :: IO ()
main = do
    arguments <- Environment.getArgs
    (name, directory) <- case arguments of
        [name, directory] -> pure (name, directory)
        _ -> do
            program <- Environment.getProgName
            fail $ "Usage: " <> program <> " NAME DIRECTORY"
    [name, directory] <- Environment.getArgs
    output <- Process.readProcess "which" [name] ""
    print output
    let source = List.dropWhileEnd Char.isSpace output
    print source
    let target = FilePath.combine directory name
    print target
    Directory.copyFile source target
