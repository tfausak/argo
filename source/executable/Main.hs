import qualified Argo
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified System.IO as IO

main :: IO ()
main = do
    contents <- ByteString.getContents
    case Argo.decode contents of
        Argo.Failure e -> fail e
        Argo.Success value -> Builder.hPutBuilder IO.stdout $ Argo.encode (value :: Argo.Value)
