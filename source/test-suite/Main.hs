{-# LANGUAGE OverloadedStrings #-}

import qualified Argo
import qualified Argo.Type.Array as Array
import qualified Argo.Type.Boolean as Boolean
import qualified Argo.Type.Null as Null
import qualified Argo.Type.Number as Number
import qualified Argo.Type.Object as Object
import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array
import qualified Data.ByteString.Builder as Builder
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Argo"
    [ Tasty.testGroup "encode" $ let encode = Builder.toLazyByteString . Argo.encode in
        [ Tasty.testGroup "Null"
            [ Tasty.testCase "null" $ do
                encode (Value.Null (Null.Null ())) ?= "null"
            ]
        , Tasty.testGroup "Boolean"
            [ Tasty.testCase "false" $ do
                encode (Value.Boolean (Boolean.Boolean False)) ?= "false"
            , Tasty.testCase "true" $ do
                encode (Value.Boolean (Boolean.Boolean True)) ?= "true"
            ]
        , Tasty.testGroup "Number"
            [ Tasty.testCase "zero" $ do
                encode (Value.Number (Number.Number 0 0)) ?= "0"
            , Tasty.testCase "positive integer" $ do
                encode (Value.Number (Number.Number 1 0)) ?= "1"
            , Tasty.testCase "negative integer" $ do
                encode (Value.Number (Number.Number (-1) 0)) ?= "-1"
            , Tasty.testCase "positive exponent" $ do
                encode (Value.Number (Number.Number 1 1)) ?= "1e1"
            , Tasty.testCase "negative exponent" $ do
                encode (Value.Number (Number.Number 1 (-1))) ?= "1e-1"
            , Tasty.testCase "multiple integer digits" $ do
                encode (Value.Number (Number.Number 12 0)) ?= "12"
            , Tasty.testCase "multiple exponent digits" $ do
                encode (Value.Number (Number.Number 1 12)) ?= "1e12"
            ]
        , Tasty.testGroup "String"
            [ Tasty.testCase "empty" $ do
                encode (Value.String (String.String "")) ?= "\"\""
            , Tasty.testCase "one character" $ do
                encode (Value.String (String.String "a")) ?= "\"a\""
            , Tasty.testCase "multiple characters" $ do
                encode (Value.String (String.String "ab")) ?= "\"ab\""
            , Tasty.testCase "quotation mark" $ do
                encode (Value.String (String.String "\"")) ?= "\"\\\"\""
            , Tasty.testCase "reverse solidus" $ do
                encode (Value.String (String.String "\\")) ?= "\"\\\\\""
            , Tasty.testCase "backspace" $ do
                encode (Value.String (String.String "\b")) ?= "\"\\b\""
            , Tasty.testCase "form feed" $ do
                encode (Value.String (String.String "\f")) ?= "\"\\f\""
            , Tasty.testCase "new line" $ do
                encode (Value.String (String.String "\n")) ?= "\"\\n\""
            , Tasty.testCase "carriage return" $ do
                encode (Value.String (String.String "\r")) ?= "\"\\r\""
            , Tasty.testCase "horizontal tabulation" $ do
                encode (Value.String (String.String "\t")) ?= "\"\\t\""
            , Tasty.testCase "null" $ do
                encode (Value.String (String.String "\x0")) ?= "\"\\u0000\""
            , Tasty.testCase "unit separator" $ do
                encode (Value.String (String.String "\x1f")) ?= "\"\\u001f\""
            , Tasty.testCase "one byte" $ do
                encode (Value.String (String.String "$")) ?= "\"$\""
            , Tasty.testCase "two bytes" $ do
                encode (Value.String (String.String "\xa2")) ?= "\"\xc2\xa2\""
            , Tasty.testCase "three bytes" $ do
                encode (Value.String (String.String "\x20ac")) ?= "\"\xe2\x82\xac\""
            , Tasty.testCase "four bytes" $ do
                encode (Value.String (String.String "\x10348")) ?= "\"\xf0\x90\x8d\x88\""
            ]
        , Tasty.testGroup "Array"
            [ Tasty.testCase "empty" $ do
                encode (Value.Array (Array.Array (array []))) ?= "[]"
            , Tasty.testCase "one element" $ do
                encode (Value.Array (Array.Array (array [Value.Number (Number.Number 1 0)]))) ?= "[1]"
            , Tasty.testCase "two elements" $ do
                encode (Value.Array (Array.Array (array [Value.Number (Number.Number 1 0), Value.Number (Number.Number 2 0)]))) ?= "[1,2]"
            ]
        , Tasty.testGroup "Object"
            [ Tasty.testCase "empty" $ do
                encode (Value.Object (Object.Object (array []))) ?= "{}"
            , Tasty.testCase "one element" $ do
                encode (Value.Object (Object.Object (array [Pair.Pair (String.String "a", Value.Number (Number.Number 1 0))]))) ?= "{\"a\":1}"
            , Tasty.testCase "two elements" $ do
                encode (Value.Object (Object.Object (array [Pair.Pair (String.String "a", Value.Number (Number.Number 1 0)), Pair.Pair (String.String "b", Value.Number (Number.Number 2 0))]))) ?= "{\"a\":1,\"b\":2}"
            ]
        ]
    ]

(?=) :: (Eq a, Show a) => a -> a -> Tasty.Assertion
(?=) = (Tasty.@?=)
infix 1 ?=

array :: [a] -> Data.Array.Array Int a
array xs = Data.Array.listArray (0, length xs - 1) xs
