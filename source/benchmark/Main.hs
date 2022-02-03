import qualified Argo
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Bench as Bench

main :: IO ()
main = Bench.defaultMain . Writer.execWriter $ do

    group "encode" $ do

        group "Null" $ do
            bench "null" Argo.Null encode

        group "Boolean" $ do
            bench "false" (Argo.Boolean False) encode
            bench "true" (Argo.Boolean True) encode

        group "Number" $ do
            bench "zero" (Argo.Number $ Argo.Decimal 0 0) encode

        group "String" $ do
            let f n = Argo.String . Text.replicate n $ Text.singleton 'a'
            bench "empty" (f 0) encode
            bench "1 character" (f 1) encode
            bench "10 characters" (f 10) encode
            bench "100 characters" (f 100) encode
            bench "1000 characters" (f 1000) encode
            bench "10000 characters" (f 10000) encode

        group "Array" $ do
            let f n = Argo.Array $ replicate n Argo.Null
            bench "empty" (f 0) encode
            bench "1 element" (f 1) encode
            bench "10 elements" (f 10) encode
            bench "100 elements" (f 100) encode
            bench "1000 elements" (f 1000) encode
            bench "10000 elements" (f 10000) encode

        group "Object" $ do
            let
                f n = Argo.Object . replicate n $ Argo.Member
                    (Argo.Name Text.empty)
                    Argo.Null
            bench "empty" (f 0) encode
            bench "1 element" (f 1) encode
            bench "10 elements" (f 10) encode
            bench "100 elements" (f 100) encode
            bench "1000 elements" (f 1000) encode
            bench "10000 elements" (f 10000) encode

    group "decode" $ do

        group "Null" $ do
            bench "null" (utf8 "null") decode

        group "Boolean" $ do
            bench "false" (utf8 "false") decode
            bench "true" (utf8 "true") decode

        group "Number" $ do
            bench "zero" (utf8 "0") decode

        group "String" $ do
            let f n = utf8 . wrap "\"" "\"" $ replicate n 'a'
            bench "empty" (f 0) decode
            bench "one byte" (utf8 "\"$\"") decode
            bench "two bytes" (utf8 "\"\xc2\xa2\"") decode
            bench "three bytes" (utf8 "\"\xe2\x82\xac\"") decode
            bench "four bytes" (utf8 "\"\xf0\x90\x8d\x88\"") decode
            bench "short escape" (utf8 "\"\\n\"") decode
            bench "long escape" (utf8 "\"\\u001f\"") decode
            bench "surrogate pair" (utf8 "\"\\ud834\\udd1e\"") decode
            bench "1 element" (f 1) decode
            bench "10 elements" (f 10) decode
            bench "100 elements" (f 100) decode
            bench "1000 elements" (f 1000) decode
            bench "10000 elements" (f 10000) decode

        group "Array" $ do
            let
                f n = utf8 . wrap "[" "]" . List.intercalate "," $ replicate
                    n
                    "null"
            bench "empty" (f 0) decode
            bench "1 element" (f 1) decode
            bench "10 elements" (f 10) decode
            bench "100 elements" (f 100) decode
            bench "1000 elements" (f 1000) decode
            bench "10000 elements" (f 10000) decode

        group "Object" $ do
            let
                f n = utf8 . wrap "{" "}" . List.intercalate "," $ replicate
                    n
                    "\"\":null"
            bench "empty" (f 0) decode
            bench "1 element" (f 1) decode
            bench "10 elements" (f 10) decode
            bench "100 elements" (f 100) decode
            bench "1000 elements" (f 1000) decode
            bench "10000 elements" (f 10000) decode

bench
    :: (DeepSeq.NFData a, DeepSeq.NFData b)
    => String
    -> a
    -> (a -> IO b)
    -> Writer.Writer [Bench.Benchmark] ()
bench name x f =
    Writer.tell
        . pure
        . Tasty.withResource (Exception.evaluate $ DeepSeq.force x) discard
        $ Bench.bench name
        . Bench.nfIO
        . (>>= f)

decode :: ByteString.ByteString -> IO Argo.Value
decode = either fail pure . Argo.decode

discard :: Applicative f => a -> f ()
discard = const $ pure ()

encode :: Argo.Value -> IO LazyByteString.ByteString
encode = pure . Builder.toLazyByteString . Argo.encode

group
    :: String
    -> Writer.Writer [Bench.Benchmark] a
    -> Writer.Writer [Bench.Benchmark] ()
group x = Writer.tell . pure . Bench.bgroup x . Writer.execWriter

utf8 :: String -> ByteString.ByteString
utf8 = Encoding.encodeUtf8 . Text.pack

wrap :: Monoid a => a -> a -> a -> a
wrap l r x = l <> x <> r
