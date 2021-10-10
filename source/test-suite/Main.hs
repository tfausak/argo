{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty.HUnit ((@?=))

import qualified Argo
import qualified Data.Array as Array
import qualified Data.ByteString.Builder as Builder
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Argo"
    [ Tasty.testGroup "encode" $ let encode = Builder.toLazyByteString . Argo.encode in
        [ Tasty.testGroup "Null"
            [ Tasty.testCase "null" $ do
                encode Argo.Null @?= "null"
            ]
        , Tasty.testGroup "Boolean"
            [ Tasty.testCase "false" $ do
                encode (Argo.Boolean False) @?= "false"
            , Tasty.testCase "true" $ do
                encode (Argo.Boolean True) @?= "true"
            ]
        , Tasty.testGroup "Number"
            [ Tasty.testCase "zero" $ do
                encode (Argo.Number 0 0) @?= "0"
            , Tasty.testCase "positive integer" $ do
                encode (Argo.Number 1 0) @?= "1"
            , Tasty.testCase "negative integer" $ do
                encode (Argo.Number (-1) 0) @?= "-1"
            , Tasty.testCase "positive exponent" $ do
                encode (Argo.Number 1 1) @?= "1e1"
            , Tasty.testCase "negative exponent" $ do
                encode (Argo.Number 1 (-1)) @?= "1e-1"
            , Tasty.testCase "multiple integer digits" $ do
                encode (Argo.Number 12 0) @?= "12"
            , Tasty.testCase "multiple exponent digits" $ do
                encode (Argo.Number 1 12) @?= "1e12"
            ]
        , Tasty.testGroup "String"
            [ Tasty.testCase "empty" $ do
                encode (Argo.String "") @?= "\"\""
            , Tasty.testCase "one character" $ do
                encode (Argo.String "a") @?= "\"a\""
            , Tasty.testCase "multiple characters" $ do
                encode (Argo.String "ab") @?= "\"ab\""
            , Tasty.testCase "quotation mark" $ do
                encode (Argo.String "\"") @?= "\"\\\"\""
            , Tasty.testCase "reverse solidus" $ do
                encode (Argo.String "\\") @?= "\"\\\\\""
            , Tasty.testCase "backspace" $ do
                encode (Argo.String "\b") @?= "\"\\b\""
            , Tasty.testCase "form feed" $ do
                encode (Argo.String "\f") @?= "\"\\f\""
            , Tasty.testCase "new line" $ do
                encode (Argo.String "\n") @?= "\"\\n\""
            , Tasty.testCase "carriage return" $ do
                encode (Argo.String "\r") @?= "\"\\r\""
            , Tasty.testCase "horizontal tabulation" $ do
                encode (Argo.String "\t") @?= "\"\\t\""
            , Tasty.testCase "null" $ do
                encode (Argo.String "\x0") @?= "\"\\u0000\""
            , Tasty.testCase "unit separator" $ do
                encode (Argo.String "\x1f") @?= "\"\\u001f\""
            , Tasty.testCase "one byte" $ do
                encode (Argo.String "$") @?= "\"$\""
            , Tasty.testCase "two bytes" $ do
                encode (Argo.String "\xa2") @?= "\"\xc2\xa2\""
            , Tasty.testCase "three bytes" $ do
                encode (Argo.String "\x20ac") @?= "\"\xe2\x82\xac\""
            , Tasty.testCase "four bytes" $ do
                encode (Argo.String "\x10348") @?= "\"\xf0\x90\x8d\x88\""
            ]
        , Tasty.testGroup "Array"
            [ Tasty.testCase "empty" $ do
                encode (Argo.Array (array [])) @?= "[]"
            , Tasty.testCase "one element" $ do
                encode (Argo.Array (array [Argo.Number 1 0])) @?= "[1]"
            , Tasty.testCase "two elements" $ do
                encode (Argo.Array (array [Argo.Number 1 0, Argo.Number 2 0])) @?= "[1,2]"
            ]
        , Tasty.testGroup "Object"
            [ Tasty.testCase "empty" $ do
                encode (Argo.Object (array [])) @?= "{}"
            , Tasty.testCase "one element" $ do
                encode (Argo.Object (array [Argo.Pair "a" $ Argo.Number 1 0])) @?= "{\"a\":1}"
            , Tasty.testCase "two elements" $ do
                encode (Argo.Object (array [Argo.Pair "a" $ Argo.Number 1 0, Argo.Pair "b" $ Argo.Number 2 0])) @?= "{\"a\":1,\"b\":2}"
            ]
        ]
    , Tasty.testGroup "decode" $ let decode = Argo.decode in
        [ Tasty.testGroup "Null"
            [ Tasty.testCase "null" $ do
                decode "null" @?= Just Argo.Null
            , Tasty.testCase "leading space" $ do
                decode " null" @?= Just Argo.Null
            , Tasty.testCase "trailing space" $ do
                decode "null " @?= Just Argo.Null
            ]
        , Tasty.testGroup "Boolean"
            [ Tasty.testCase "false" $ do
                decode "false" @?= Just (Argo.Boolean False)
            , Tasty.testCase "true" $ do
                decode "true" @?= Just (Argo.Boolean True)
            ]
        , Tasty.testGroup "Number"
            [ Tasty.testCase "zero" $ do
                decode "0" @?= Just (Argo.Number 0 0)
            , Tasty.testCase "positive integer" $ do
                decode "1" @?= Just (Argo.Number 1 0)
            , Tasty.testCase "multiple integer digits" $ do
                decode "12" @?= Just (Argo.Number 12 0)
            , Tasty.testCase "negative integer" $ do
                decode "-1" @?= Just (Argo.Number (-1) 0)
            , Tasty.testCase "fraction" $ do
                decode "0.1" @?= Just (Argo.Number 1 (-1))
            , Tasty.testCase "multiple fraction digits" $ do
                decode "0.12" @?= Just (Argo.Number 12 (-2))
            , Tasty.testCase "leading zero fraction" $ do
                decode "0.01" @?= Just (Argo.Number 1 (-2))
            , Tasty.testCase "positive exponent" $ do
                decode "1e1" @?= Just (Argo.Number 1 1)
            , Tasty.testCase "capital exponent" $ do
                decode "1E1" @?= Just (Argo.Number 1 1)
            , Tasty.testCase "leading zero exponent" $ do
                decode "1e01" @?= Just (Argo.Number 1 1)
            , Tasty.testCase "explicit positive exponent" $ do
                decode "1e+1" @?= Just (Argo.Number 1 1)
            , Tasty.testCase "negative exponent" $ do
                decode "1e-1" @?= Just (Argo.Number 1 (-1))
            , Tasty.testCase "multiple exponent digits" $ do
                decode "1e12" @?= Just (Argo.Number 1 12)
            , Tasty.testCase "kitchen sink" $ do
                decode "12.34e56" @?= Just (Argo.Number 1234 54)
            , Tasty.testCase "normalized" $ do
                decode "10" @?= Just (Argo.Number 1 1)
            ]
        , Tasty.testGroup "String"
            [ Tasty.testCase "empty" $ do
                decode "\"\"" @?= Just (Argo.String "")
            , Tasty.testCase "one character" $ do
                decode "\"a\"" @?= Just (Argo.String "a")
            , Tasty.testCase "multiple characters" $ do
                decode "\"ab\"" @?= Just (Argo.String "ab")
            , Tasty.testCase "unnecessary escape" $ do
                decode "\"\\u0020\"" @?= Just (Argo.String " ")
            , Tasty.testCase "quotation mark" $ do
                decode "\"\\\"\"" @?= Just (Argo.String "\"")
            , Tasty.testCase "reverse solidus" $ do
                decode "\"\\\\\"" @?= Just (Argo.String "\\")
            , Tasty.testCase "solidus" $ do
                decode "\"\\/\"" @?= Just (Argo.String "/")
            , Tasty.testCase "backspace" $ do
                decode "\"\\b\"" @?= Just (Argo.String "\b")
            , Tasty.testCase "form feed" $ do
                decode "\"\\f\"" @?= Just (Argo.String "\f")
            , Tasty.testCase "new line" $ do
                decode "\"\\n\"" @?= Just (Argo.String "\n")
            , Tasty.testCase "carriage return" $ do
                decode "\"\\r\"" @?= Just (Argo.String "\r")
            , Tasty.testCase "horizontal tabulation" $ do
                decode "\"\\t\"" @?= Just (Argo.String "\t")
            , Tasty.testCase "null" $ do
                decode "\"\\u0000\"" @?= Just (Argo.String "\x0")
            , Tasty.testCase "unit separator" $ do
                decode "\"\\u001f\"" @?= Just (Argo.String "\x1f")
            , Tasty.testCase "one byte" $ do
                decode "\"$\"" @?= Just (Argo.String "$")
            , Tasty.testCase "two bytes" $ do
                decode "\"\xc2\xa2\"" @?= Just (Argo.String "\xa2")
            , Tasty.testCase "three bytes" $ do
                decode "\"\xe2\x82\xac\"" @?= Just (Argo.String "\x20ac")
            , Tasty.testCase "four bytes" $ do
                decode "\"\xf0\x90\x8d\x88\"" @?= Just (Argo.String "\x10348")
            , Tasty.testCase "surrogate pair" $ do
                decode "\"\\ud834\\udd1e\"" @?= Just (Argo.String "\x1d11e")
            , Tasty.testCase "unpaired high surrogate" $ do
                decode "\"\\ud834\"" @?= Just (Argo.String "\xfffd")
            , Tasty.testCase "unpaired low surrogate" $ do
                decode "\"\\udd1e\"" @?= Just (Argo.String "\xfffd")
            ]
        , Tasty.testGroup "Array"
            [ Tasty.testCase "empty" $ do
                decode "[]" @?= Just (Argo.Array $ array [])
            , Tasty.testCase "one element" $ do
                decode "[1]" @?= Just (Argo.Array $ array [Argo.Number 1 0])
            , Tasty.testCase "two elements" $ do
                decode "[1,2]" @?= Just (Argo.Array $ array [Argo.Number 1 0, Argo.Number 2 0])
            ]
        , Tasty.testGroup "Object"
            [ Tasty.testCase "empty" $ do
                decode "{}" @?= Just (Argo.Object $ array [])
            , Tasty.testCase "one element" $ do
                decode "{\"a\":1}" @?= Just (Argo.Object $ array [Argo.Pair "a" $ Argo.Number 1 0])
            , Tasty.testCase "two elements" $ do
                decode "{\"a\":1,\"b\":2}" @?= Just (Argo.Object $ array [Argo.Pair "a" $ Argo.Number 1 0, Argo.Pair "b" $ Argo.Number 2 0])
            ]
        ]
    ]

array :: [a] -> Array.Array Int a
array xs = Array.listArray (0, length xs - 1) xs
