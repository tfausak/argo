{-# LANGUAGE OverloadedStrings #-}

import qualified Argo
import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array
import qualified Data.ByteString.Builder as Builder
import qualified Test.Tasty.Bench as Tasty

main :: IO ()
main = Tasty.defaultMain
    [ Tasty.bgroup "encode" $ let encode = Builder.toLazyByteString . Argo.encode in
        [ Tasty.bgroup "Null"
            [ Tasty.bench "null" . Tasty.nf encode . Value.Null $ Null.Null ()
            ]
        , Tasty.bgroup "Boolean"
            [ Tasty.bench "false" . Tasty.nf encode . Value.Boolean $ Boolean.Boolean False
            ]
        , Tasty.bgroup "Number"
            [ Tasty.bench "zero" . Tasty.nf encode . Value.Number $ Number.Number 0 0
            ]
        , Tasty.bgroup "String"
            [ Tasty.bench "empty" . Tasty.nf encode . Value.String $ String.String ""
            ]
        , Tasty.bgroup "Array"
            [ Tasty.bench "empty" . Tasty.nf encode . Value.Array . Array.Array $ array []
            ]
        , Tasty.bgroup "Object"
            [ Tasty.bench "empty" . Tasty.nf encode . Value.Object . Object.Object $ array []
            ]
        ]
    ]

array :: [a] -> Data.Array.Array Int a
array xs = Data.Array.listArray (0, length xs - 1) xs
