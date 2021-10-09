{-# LANGUAGE OverloadedStrings #-}

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
                encode Argo.Null ?= "null"
            ]
        , Tasty.testGroup "Boolean"
            [ Tasty.testCase "false" $ do
                encode (Argo.Boolean False) ?= "false"
            , Tasty.testCase "true" $ do
                encode (Argo.Boolean True) ?= "true"
            ]
        , Tasty.testGroup "Number"
            [ Tasty.testCase "zero" $ do
                encode (Argo.Number 0 0) ?= "0"
            , Tasty.testCase "positive integer" $ do
                encode (Argo.Number 1 0) ?= "1"
            , Tasty.testCase "negative integer" $ do
                encode (Argo.Number (-1) 0) ?= "-1"
            , Tasty.testCase "positive exponent" $ do
                encode (Argo.Number 1 1) ?= "1e1"
            , Tasty.testCase "negative exponent" $ do
                encode (Argo.Number 1 (-1)) ?= "1e-1"
            , Tasty.testCase "multiple integer digits" $ do
                encode (Argo.Number 12 0) ?= "12"
            , Tasty.testCase "multiple exponent digits" $ do
                encode (Argo.Number 1 12) ?= "1e12"
            ]
        , Tasty.testGroup "String"
            [ Tasty.testCase "empty" $ do
                encode (Argo.String "") ?= "\"\""
            , Tasty.testCase "one character" $ do
                encode (Argo.String "a") ?= "\"a\""
            , Tasty.testCase "multiple characters" $ do
                encode (Argo.String "ab") ?= "\"ab\""
            , Tasty.testCase "quotation mark" $ do
                encode (Argo.String "\"") ?= "\"\\\"\""
            , Tasty.testCase "reverse solidus" $ do
                encode (Argo.String "\\") ?= "\"\\\\\""
            , Tasty.testCase "backspace" $ do
                encode (Argo.String "\b") ?= "\"\\b\""
            , Tasty.testCase "form feed" $ do
                encode (Argo.String "\f") ?= "\"\\f\""
            , Tasty.testCase "new line" $ do
                encode (Argo.String "\n") ?= "\"\\n\""
            , Tasty.testCase "carriage return" $ do
                encode (Argo.String "\r") ?= "\"\\r\""
            , Tasty.testCase "horizontal tabulation" $ do
                encode (Argo.String "\t") ?= "\"\\t\""
            , Tasty.testCase "null" $ do
                encode (Argo.String "\x0") ?= "\"\\u0000\""
            , Tasty.testCase "unit separator" $ do
                encode (Argo.String "\x1f") ?= "\"\\u001f\""
            , Tasty.testCase "one byte" $ do
                encode (Argo.String "$") ?= "\"$\""
            , Tasty.testCase "two bytes" $ do
                encode (Argo.String "\xa2") ?= "\"\xc2\xa2\""
            , Tasty.testCase "three bytes" $ do
                encode (Argo.String "\x20ac") ?= "\"\xe2\x82\xac\""
            , Tasty.testCase "four bytes" $ do
                encode (Argo.String "\x10348") ?= "\"\xf0\x90\x8d\x88\""
            ]
        , Tasty.testGroup "Array"
            [ Tasty.testCase "empty" $ do
                encode (Argo.Array (array [])) ?= "[]"
            , Tasty.testCase "one element" $ do
                encode (Argo.Array (array [Argo.Number 1 0])) ?= "[1]"
            , Tasty.testCase "two elements" $ do
                encode (Argo.Array (array [Argo.Number 1 0, Argo.Number 2 0])) ?= "[1,2]"
            ]
        , Tasty.testGroup "Object"
            [ Tasty.testCase "empty" $ do
                encode (Argo.Object (array [])) ?= "{}"
            , Tasty.testCase "one element" $ do
                encode (Argo.Object (array [Argo.Pair "a" $ Argo.Number 1 0])) ?= "{\"a\":1}"
            , Tasty.testCase "two elements" $ do
                encode (Argo.Object (array [Argo.Pair "a" $ Argo.Number 1 0, Argo.Pair "b" $ Argo.Number 2 0])) ?= "{\"a\":1,\"b\":2}"
            ]
        ]
    ]

(?=) :: (Eq a, Show a) => a -> a -> Tasty.Assertion
(?=) = (Tasty.@?=)
infix 1 ?=

array :: [a] -> Array.Array Int a
array xs = Array.listArray (0, length xs - 1) xs
