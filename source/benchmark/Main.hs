{-# LANGUAGE OverloadedStrings #-}

import qualified Argo
import qualified Data.Array
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as Text
import qualified Test.Tasty.Bench as Tasty

main :: IO ()
main = Tasty.defaultMain
    [ Tasty.bgroup "encode" $ let encode = Builder.toLazyByteString . Argo.encode in
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
            , Tasty.bench "1 element" . Tasty.nf encode . Argo.Object . array . replicate 1 $ Argo.Pair "" Argo.Null
            , Tasty.bench "10 elements" . Tasty.nf encode . Argo.Object . array . replicate 10 $ Argo.Pair "" Argo.Null
            , Tasty.bench "100 elements" . Tasty.nf encode . Argo.Object . array . replicate 100 $ Argo.Pair "" Argo.Null
            , Tasty.bench "1000 elements" . Tasty.nf encode . Argo.Object . array . replicate 1000 $ Argo.Pair "" Argo.Null
            , Tasty.bench "10000 elements" . Tasty.nf encode . Argo.Object . array . replicate 10000 $ Argo.Pair "" Argo.Null
            ]
        ]
    , Tasty.bgroup "decode" $ let decode = Argo.decode in
        [ Tasty.bgroup "Null"
            [ Tasty.bench "null" $ Tasty.whnf decode "null"
            ]
        , Tasty.bgroup "Boolean"
            [ Tasty.bench "false" $ Tasty.whnf decode "false"
            ]
        , Tasty.bgroup "Number"
            [ Tasty.bench "zero" $ Tasty.whnf decode "0"
            ]
        , Tasty.bgroup "String"
            [ Tasty.bench "empty" $ Tasty.whnf decode "\"\""
            ]
        , Tasty.bgroup "Array"
            [ Tasty.bench "empty" $ Tasty.whnf decode "[]"
            ]
        , Tasty.bgroup "Object"
            [ Tasty.bench "empty" $ Tasty.whnf decode "{}"
            ]
        ]
    ]

array :: [a] -> Data.Array.Array Int a
array xs = Data.Array.listArray (0, length xs - 1) xs
