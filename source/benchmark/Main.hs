{-# LANGUAGE OverloadedStrings #-}

import qualified Argo
import qualified Data.Array
import qualified Data.ByteString.Builder as Builder
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
            ]
        , Tasty.bgroup "Array"
            [ Tasty.bench "empty" . Tasty.nf encode . Argo.Array $ array []
            ]
        , Tasty.bgroup "Object"
            [ Tasty.bench "empty" . Tasty.nf encode . Argo.Object $ array []
            ]
        ]
    ]

array :: [a] -> Data.Array.Array Int a
array xs = Data.Array.listArray (0, length xs - 1) xs
