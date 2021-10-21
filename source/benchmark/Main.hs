{-# LANGUAGE OverloadedStrings #-}

import qualified Argo
import qualified Data.Array
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Test.Tasty.Bench as Tasty

main :: IO ()
main = Tasty.defaultMain
    [ Tasty.bgroup "encode" $ let encode = Builder.toLazyByteString . Argo.encode :: Argo.Value -> LazyByteString.ByteString in
        [ Tasty.bgroup "Null"
            [ Tasty.bench "null" $ Tasty.nf encode Argo.Null
            ]
        , Tasty.bgroup "Boolean"
            [ Tasty.bench "false" . Tasty.nf encode $ Argo.Boolean False
            ]
        , Tasty.bgroup "Number"
            [ Tasty.bench "zero" . Tasty.nf encode $ Argo.Number 0 0
            ]
        , Tasty.bgroup "String"
            [ Tasty.bench "empty" . Tasty.nf encode $ Argo.String ""
            , Tasty.bench "1 character" . Tasty.nf encode . Argo.String . Text.pack $ replicate 1 'a'
            , Tasty.bench "10 characters" . Tasty.nf encode . Argo.String . Text.pack $ replicate 10 'a'
            , Tasty.bench "100 characters" . Tasty.nf encode . Argo.String . Text.pack $ replicate 100 'a'
            , Tasty.bench "1000 characters" . Tasty.nf encode . Argo.String . Text.pack $ replicate 1000 'a'
            , Tasty.bench "10000 characters" . Tasty.nf encode . Argo.String . Text.pack $ replicate 10000 'a'
            ]
        , Tasty.bgroup "Array"
            [ Tasty.bench "empty" . Tasty.nf encode . Argo.Array $ array []
            , Tasty.bench "1 element" . Tasty.nf encode . Argo.Array . array $ replicate 1 Argo.Null
            , Tasty.bench "10 elements" . Tasty.nf encode . Argo.Array . array $ replicate 10 Argo.Null
            , Tasty.bench "100 elements" . Tasty.nf encode . Argo.Array . array $ replicate 100 Argo.Null
            , Tasty.bench "1000 elements" . Tasty.nf encode . Argo.Array . array $ replicate 1000 Argo.Null
            , Tasty.bench "10000 elements" . Tasty.nf encode . Argo.Array . array $ replicate 10000 Argo.Null
            ]
        , Tasty.bgroup "Object"
            [ Tasty.bench "empty" . Tasty.nf encode . Argo.Object $ array []
            , Tasty.bench "1 element" . Tasty.nf encode . Argo.Object . array . replicate 1 $ Argo.Member "" Argo.Null
            , Tasty.bench "10 elements" . Tasty.nf encode . Argo.Object . array . replicate 10 $ Argo.Member "" Argo.Null
            , Tasty.bench "100 elements" . Tasty.nf encode . Argo.Object . array . replicate 100 $ Argo.Member "" Argo.Null
            , Tasty.bench "1000 elements" . Tasty.nf encode . Argo.Object . array . replicate 1000 $ Argo.Member "" Argo.Null
            , Tasty.bench "10000 elements" . Tasty.nf encode . Argo.Object . array . replicate 10000 $ Argo.Member "" Argo.Null
            ]
        ]
    , Tasty.bgroup "decode" $ let decode = resultToMaybe . Argo.decode :: ByteString.ByteString -> Maybe Argo.Value in
        [ Tasty.bgroup "Null"
            [ Tasty.bench "null" $ Tasty.nf decode "null"
            ]
        , Tasty.bgroup "Boolean"
            [ Tasty.bench "false" $ Tasty.nf decode "false"
            ]
        , Tasty.bgroup "Number"
            [ Tasty.bench "zero" $ Tasty.nf decode "0"
            ]
        , Tasty.bgroup "String"
            [ Tasty.bench "empty" $ Tasty.nf decode "\"\""
            , Tasty.bench "one byte" $ Tasty.nf decode "\"$\""
            , Tasty.bench "two bytes" $ Tasty.nf decode "\"\xc2\xa2\""
            , Tasty.bench "three bytes" $ Tasty.nf decode "\"\xe2\x82\xac\""
            , Tasty.bench "four bytes" $ Tasty.nf decode "\"\xf0\x90\x8d\x88\""
            , Tasty.bench "short escape" $ Tasty.nf decode "\"\\n\""
            , Tasty.bench "long escape" $ Tasty.nf decode "\"\\u001f\""
            , Tasty.bench "surrogate pair" $ Tasty.nf decode "\"\\ud834\\udd1e\""
            ]
        , Tasty.bgroup "Array"
            [ Tasty.bench "empty" $ Tasty.nf decode "[]"
            ]
        , Tasty.bgroup "Object"
            [ Tasty.bench "empty" $ Tasty.nf decode "{}"
            ]
        ]
    ]

array :: [a] -> Data.Array.Array Int a
array xs = Data.Array.listArray (0, length xs - 1) xs

resultToMaybe :: Argo.Result a -> Maybe a
resultToMaybe r = case r of
    Argo.Failure _ -> Nothing
    Argo.Success x -> Just x
