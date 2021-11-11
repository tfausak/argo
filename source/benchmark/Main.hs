{-# LANGUAGE OverloadedStrings #-}

import qualified Argo
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
            [ Tasty.bench "zero" . Tasty.nf encode . Argo.Number $ Argo.decimal 0 0
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
            [ Tasty.bench "empty" . Tasty.nf encode $ Argo.Array []
            , Tasty.bench "1 element" . Tasty.nf encode . Argo.Array $ replicate 1 Argo.Null
            , Tasty.bench "10 elements" . Tasty.nf encode . Argo.Array $ replicate 10 Argo.Null
            , Tasty.bench "100 elements" . Tasty.nf encode . Argo.Array $ replicate 100 Argo.Null
            , Tasty.bench "1000 elements" . Tasty.nf encode . Argo.Array $ replicate 1000 Argo.Null
            , Tasty.bench "10000 elements" . Tasty.nf encode . Argo.Array $ replicate 10000 Argo.Null
            ]
        , Tasty.bgroup "Object"
            [ Tasty.bench "empty" . Tasty.nf encode $ Argo.Object []
            , Tasty.bench "1 element" . Tasty.nf encode . Argo.Object . replicate 1 $ Argo.Member (Argo.Name "") Argo.Null
            , Tasty.bench "10 elements" . Tasty.nf encode . Argo.Object . replicate 10 $ Argo.Member (Argo.Name "") Argo.Null
            , Tasty.bench "100 elements" . Tasty.nf encode . Argo.Object . replicate 100 $ Argo.Member (Argo.Name "") Argo.Null
            , Tasty.bench "1000 elements" . Tasty.nf encode . Argo.Object . replicate 1000 $ Argo.Member (Argo.Name "") Argo.Null
            , Tasty.bench "10000 elements" . Tasty.nf encode . Argo.Object . replicate 10000 $ Argo.Member (Argo.Name "") Argo.Null
            ]
        ]
    , Tasty.bgroup "decode" $ let decode = Argo.decode :: ByteString.ByteString -> Either String Argo.Value in
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
            , Tasty.bench "1 character" . Tasty.nf decode $ "\"" <> ByteString.replicate 1 0x61 <> "\""
            , Tasty.bench "10 characters" . Tasty.nf decode $ "\"" <> ByteString.replicate 10 0x61 <> "\""
            , Tasty.bench "100 characters" . Tasty.nf decode $ "\"" <> ByteString.replicate 100 0x61 <> "\""
            , Tasty.bench "1000 characters" . Tasty.nf decode $ "\"" <> ByteString.replicate 1000 0x61 <> "\""
            , Tasty.bench "10000 characters" . Tasty.nf decode $ "\"" <> ByteString.replicate 10000 0x61 <> "\""
            ]
        , Tasty.bgroup "Array"
            [ Tasty.bench "empty" $ Tasty.nf decode "[]"
            , Tasty.bench "1 element" $ Tasty.nf decode "[null]"
            , Tasty.bench "10 elements" . Tasty.nf decode $ "[null" <> ByteString.pack (take (5 * 9) $ cycle [0x2c, 0x6e, 0x75, 0x6c, 0x6c]) <> "]"
            , Tasty.bench "100 elements" . Tasty.nf decode $ "[null" <> ByteString.pack (take (5 * 99) $ cycle [0x2c, 0x6e, 0x75, 0x6c, 0x6c]) <> "]"
            , Tasty.bench "1000 elements" . Tasty.nf decode $ "[null" <> ByteString.pack (take (5 * 999) $ cycle [0x2c, 0x6e, 0x75, 0x6c, 0x6c]) <> "]"
            , Tasty.bench "10000 elements" . Tasty.nf decode $ "[null" <> ByteString.pack (take (5 * 9999) $ cycle [0x2c, 0x6e, 0x75, 0x6c, 0x6c]) <> "]"
            ]
        , Tasty.bgroup "Object"
            [ Tasty.bench "empty" $ Tasty.nf decode "{}"
            , Tasty.bench "1 element" $ Tasty.nf decode "{\"\":null}"
            , Tasty.bench "10 elements" . Tasty.nf decode $ "{\"\":null" <> ByteString.pack (take (5 * 9) $ cycle [0x2c, 0x22, 0x22, 0x3a, 0x6e, 0x75, 0x6c, 0x6c]) <> "}"
            , Tasty.bench "100 elements" . Tasty.nf decode $ "{\"\":null" <> ByteString.pack (take (5 * 99) $ cycle [0x2c, 0x22, 0x22, 0x3a, 0x6e, 0x75, 0x6c, 0x6c]) <> "}"
            , Tasty.bench "1000 elements" . Tasty.nf decode $ "{\"\":null" <> ByteString.pack (take (5 * 999) $ cycle [0x2c, 0x22, 0x22, 0x3a, 0x6e, 0x75, 0x6c, 0x6c]) <> "}"
            , Tasty.bench "10000 elements" . Tasty.nf decode $ "{\"\":null" <> ByteString.pack (take (5 * 9999) $ cycle [0x2c, 0x22, 0x22, 0x3a, 0x6e, 0x75, 0x6c, 0x6c]) <> "}"
            ]
        ]
    , Tasty.bgroup "Pointer"
        [ Tasty.bench "decode" $ Tasty.nf Argo.decodePointer ""
        , Tasty.bench "encode" . Tasty.nf Builder.toLazyByteString . Argo.encodePointer $ Argo.Pointer []
        ]
    ]
