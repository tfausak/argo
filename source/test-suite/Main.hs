{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.GenValidity.Map ()
import Data.GenValidity.Text ()
import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.QuickCheck ((===))

import qualified Argo
import qualified Argo.Type.String as String
import qualified Data.Array as Array
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.GenValidity as GenValidity
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Word as Word
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import qualified Test.Tasty.QuickCheck as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Argo"
    [ Tasty.testGroup "encode" $ let encode = Builder.toLazyByteString . Argo.encode :: Argo.Value -> LazyByteString.ByteString in
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
    , Tasty.testGroup "decode" $ let decode = resultToMaybe . Argo.decode :: ByteString.ByteString -> Maybe Argo.Value in
        [ Tasty.testGroup "Null"
            [ Tasty.testCase "null" $ do
                decode "null" @?= Just Argo.Null
            , Tasty.testCase "leading space" $ do
                decode " null" @?= Just Argo.Null
            , Tasty.testCase "trailing space" $ do
                decode "null " @?= Just Argo.Null
            , Tasty.testCase "trailing new line" $ do
                decode "null\n" @?= Just Argo.Null
            , Tasty.testCase "trailing carriage return" $ do
                decode "null\r" @?= Just Argo.Null
            , Tasty.testCase "trailing horizontal tab" $ do
                decode "null\t" @?= Just Argo.Null
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
            , Tasty.testCase "negative zero" $ do
                decode "-0" @?= Just (Argo.Number 0 0)
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
            , Tasty.testCase "leading zero" $ do
                decode "01" @?= Nothing
            , Tasty.testCase "trailing fraction" $ do
                decode "1." @?= Nothing
            , Tasty.testCase "leading fraction" $ do
                decode ".1" @?= Nothing
            , Tasty.testCase "trailing exponent" $ do
                decode "1e" @?= Nothing
            , Tasty.testCase "leading exponent" $ do
                decode "e1" @?= Nothing
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
            , Tasty.testCase "capital long escape" $ do
                decode "\"\\u001F\"" @?= Just (Argo.String "\x1f")
            , Tasty.testCase "digits after long escape" $ do
                decode "\"\\u00205\"" @?= Just (Argo.String " 5")
            , Tasty.testCase "one byte" $ do
                decode "\"$\"" @?= Just (Argo.String "$")
            , Tasty.testCase "one byte escaped" $ do
                decode "\"\\u0024\"" @?= Just (Argo.String "$")
            , Tasty.testCase "two bytes" $ do
                decode "\"\xc2\xa2\"" @?= Just (Argo.String "\xa2")
            , Tasty.testCase "two bytes escaped" $ do
                decode "\"\\u00a2\"" @?= Just (Argo.String "\xa2")
            , Tasty.testCase "three bytes" $ do
                decode "\"\xe2\x82\xac\"" @?= Just (Argo.String "\x20ac")
            , Tasty.testCase "three bytes escaped" $ do
                decode "\"\\u20ac\"" @?= Just (Argo.String "\x20ac")
            , Tasty.testCase "four bytes" $ do
                decode "\"\xf0\x90\x8d\x88\"" @?= Just (Argo.String "\x10348")
            , Tasty.testCase "surrogate pair" $ do
                decode "\"\\ud834\\udd1e\"" @?= Just (Argo.String "\x1d11e")
            , Tasty.testCase "unpaired high surrogate" $ do
                decode "\"\\ud800\"" @?= Just (Argo.String "\xfffd")
            , Tasty.testCase "unpaired low surrogate" $ do
                decode "\"\\udc00\"" @?= Just (Argo.String "\xfffd")
            , Tasty.testCase "delete" $ do
                decode "\"\x7f\"" @?= Just (Argo.String "\x7f")
            , Tasty.testCase "invalid short escape" $ do
                decode "\"\\z\"" @?= Nothing
            , Tasty.testCase "capital short escape" $ do
                decode "\"\\T\"" @?= Nothing
            , Tasty.testCase "invalid long escape" $ do
                decode "\"\\uwxyz\"" @?= Nothing
            , Tasty.testCase "incomplete long escape" $ do
                decode "\"\\u00\"" @?= Nothing
            , Tasty.testCase "unescaped control character" $ do
                decode "\"\n\"" @?= Nothing
            , Tasty.testCase "unterminated" $ do
                decode "\"" @?= Nothing
            , Tasty.testCase "invalid UTF-8 byte" $ do
                decode "\"\xff\"" @?= Nothing
            ]
        , Tasty.testGroup "Array"
            [ Tasty.testCase "empty" $ do
                decode "[]" @?= Just (Argo.Array $ array [])
            , Tasty.testCase "one element" $ do
                decode "[1]" @?= Just (Argo.Array $ array [Argo.Number 1 0])
            , Tasty.testCase "two elements" $ do
                decode "[1,2]" @?= Just (Argo.Array $ array [Argo.Number 1 0, Argo.Number 2 0])
            , Tasty.testCase "nested" $ do
                decode "[1,[2]]" @?= Just (Argo.Array $ array [Argo.Number 1 0, Argo.Array $ array [Argo.Number 2 0]])
            , Tasty.testCase "not closed" $ do
                decode "[" @?= Nothing
            , Tasty.testCase "not opened" $ do
                decode "]" @?= Nothing
            , Tasty.testCase "leading comma" $ do
                decode "[,1]" @?= Nothing
            , Tasty.testCase "trailing comma" $ do
                decode "[1,]" @?= Nothing
            , Tasty.testCase "consecutive commas" $ do
                decode "[1,,2]" @?= Nothing
            , Tasty.testCase "like an object" $ do
                decode "[\"a\":1]" @?= Nothing
            ]
        , Tasty.testGroup "Object"
            [ Tasty.testCase "empty" $ do
                decode "{}" @?= Just (Argo.Object $ array [])
            , Tasty.testCase "one element" $ do
                decode "{\"a\":1}" @?= Just (Argo.Object $ array [Argo.Pair "a" $ Argo.Number 1 0])
            , Tasty.testCase "two elements" $ do
                decode "{\"a\":1,\"b\":2}" @?= Just (Argo.Object $ array [Argo.Pair "a" $ Argo.Number 1 0, Argo.Pair "b" $ Argo.Number 2 0])
            , Tasty.testCase "nested" $ do
                decode "{\"a\":{\"b\":2}}" @?= Just (Argo.Object $ array [Argo.Pair "a" . Argo.Object $ array [Argo.Pair "b" $ Argo.Number 2 0]])
            , Tasty.testCase "not closed" $ do
                decode "{" @?= Nothing
            , Tasty.testCase "not opened" $ do
                decode "}" @?= Nothing
            , Tasty.testCase "leading comma" $ do
                decode "{,\"a\":1}" @?= Nothing
            , Tasty.testCase "trailing comma" $ do
                decode "{\"a\":1,}" @?= Nothing
            , Tasty.testCase "consecutive commas" $ do
                decode "{\"a\":1,,\"b\":2}" @?= Nothing
            , Tasty.testCase "missing key" $ do
                decode "{:1}" @?= Nothing
            , Tasty.testCase "missing value" $ do
                decode "{\"a\":}" @?= Nothing
            , Tasty.testCase "missing separator" $ do
                decode "{\"a\"1}" @?= Nothing
            , Tasty.testCase "duplicate separator" $ do
                decode "{\"a\"::1}" @?= Nothing
            , Tasty.testCase "like an array" $ do
                decode "{1}" @?= Nothing
            , Tasty.testCase "non-string key" $ do
                decode "{1:2}" @?= Nothing
            ]
        ]
    , Tasty.testGroup "fromValue" $
        let
            fromValue :: Argo.FromValue a => Argo.Value -> Maybe a
            fromValue = resultToMaybe . Argo.fromValue
        in
        [ Tasty.testCase "Value" $ do
            fromValue Argo.Null @?= Just Argo.Null
        , Tasty.testCase "Bool" $ do
            fromValue (Argo.Boolean False) @?= Just False
        , Tasty.testCase "Char" $ do
            fromValue (Argo.String "a") @?= Just 'a'
        , Tasty.testCase "Int" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Int)
        , Tasty.testCase "Int8" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Int.Int8)
        , Tasty.testCase "Int16" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Int.Int16)
        , Tasty.testCase "Int32" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Int.Int32)
        , Tasty.testCase "Int64" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Int.Int64)
        , Tasty.testCase "Word" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Word)
        , Tasty.testCase "Word8" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Word.Word8)
        , Tasty.testCase "Word16" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Word.Word16)
        , Tasty.testCase "Word32" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Word.Word32)
        , Tasty.testCase "Word64" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Word.Word64)
        , Tasty.testCase "Integer" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Integer)
        , Tasty.testCase "Float" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Float)
        , Tasty.testCase "Double" $ do
            fromValue (Argo.Number 0 0) @?= Just (0 :: Double)
        , Tasty.testCase "String" $ do
            fromValue (Argo.String "") @?= Just ("" :: String)
        , Tasty.testCase "Text" $ do
            fromValue (Argo.String "") @?= Just ("" :: Text.Text)
        , Tasty.testCase "LazyText" $ do
            fromValue (Argo.String "") @?= Just ("" :: LazyText.Text)
        , Tasty.testCase "Maybe a" $ do
            fromValue (Argo.Boolean False) @?= Just (Just False)
        , Tasty.testCase "()" $ do
            fromValue (Argo.Array $ array []) @?= Just ()
        , Tasty.testCase "(a, b)" $ do
            fromValue (Argo.Array $ array [Argo.Boolean False, Argo.String "a"]) @?= Just (False, 'a')
        , Tasty.testCase "Array Int a" $ do
            fromValue (Argo.Array $ array []) @?= Just (array [] :: Array.Array Int Bool)
        , Tasty.testCase "[a]" $ do
            fromValue (Argo.Array $ array []) @?= Just ([] :: [Bool])
        , Tasty.testCase "NonEmpty a" $ do
            fromValue (Argo.Array $ array [Argo.Boolean False]) @?= Just (False :| [])
        , Tasty.testCase "Map Text a" $ do
            fromValue (Argo.Object $ array [Argo.Pair "a" $ Argo.Boolean False]) @?= Just (Map.fromList [("a" :: Text.Text, False)])
        ]
    , Tasty.testGroup "toValue"
        [ Tasty.testCase "Value" $ do
            Argo.toValue Argo.Null @?= Argo.Null
        , Tasty.testCase "Bool" $ do
            Argo.toValue False @?= Argo.Boolean False
        , Tasty.testCase "Char" $ do
            Argo.toValue 'a' @?= Argo.String "a"
        , Tasty.testCase "Int" $ do
            Argo.toValue (0 :: Int) @?= Argo.Number 0 0
        , Tasty.testCase "Int8" $ do
            Argo.toValue (0 :: Int.Int8) @?= Argo.Number 0 0
        , Tasty.testCase "Int16" $ do
            Argo.toValue (0 :: Int.Int16) @?= Argo.Number 0 0
        , Tasty.testCase "Int32" $ do
            Argo.toValue (0 :: Int.Int32) @?= Argo.Number 0 0
        , Tasty.testCase "Int64" $ do
            Argo.toValue (0 :: Int.Int64) @?= Argo.Number 0 0
        , Tasty.testCase "Word" $ do
            Argo.toValue (0 :: Word) @?= Argo.Number 0 0
        , Tasty.testCase "Word8" $ do
            Argo.toValue (0 :: Word.Word8) @?= Argo.Number 0 0
        , Tasty.testCase "Word16" $ do
            Argo.toValue (0 :: Word.Word16) @?= Argo.Number 0 0
        , Tasty.testCase "Word32" $ do
            Argo.toValue (0 :: Word.Word32) @?= Argo.Number 0 0
        , Tasty.testCase "Word64" $ do
            Argo.toValue (0 :: Word.Word64) @?= Argo.Number 0 0
        , Tasty.testCase "Integer" $ do
            Argo.toValue (0 :: Integer) @?= Argo.Number 0 0
        , Tasty.testCase "Float" $ do
            Argo.toValue (0 :: Float) @?= Argo.Number 0 0
        , Tasty.testCase "Double" $ do
            Argo.toValue (0 :: Double) @?= Argo.Number 0 0
        , Tasty.testCase "String" $ do
            Argo.toValue ("" :: String) @?= Argo.String ""
        , Tasty.testCase "Text" $ do
            Argo.toValue ("" :: Text.Text) @?= Argo.String ""
        , Tasty.testCase "LazyText" $ do
            Argo.toValue ("" :: LazyText.Text) @?= Argo.String ""
        , Tasty.testCase "Maybe a" $ do
            Argo.toValue (Just False) @?= Argo.Boolean False
        , Tasty.testCase "()" $ do
            Argo.toValue () @?= Argo.Array (array [])
        , Tasty.testCase "(a, b)" $ do
            Argo.toValue (False, 'a') @?= Argo.Array (array [Argo.Boolean False, Argo.String "a"])
        , Tasty.testCase "Array Int a" $ do
            Argo.toValue (array [] :: Array.Array Int Bool) @?= Argo.Array (array [])
        , Tasty.testCase "[a]" $ do
            Argo.toValue ([] :: [Bool]) @?= Argo.Array (array [])
        , Tasty.testCase "NonEmpty a" $ do
            Argo.toValue (False :| []) @?= Argo.Array (array [Argo.Boolean False])
        , Tasty.testCase "Map Text a" $ do
            Argo.toValue (Map.fromList [("a" :: Text.Text, False)]) @?= Argo.Object (array [Argo.Pair "a" $ Argo.Boolean False])
        ]
    , Tasty.testGroup "quasi quoter"
        [ Tasty.testCase "Null" $ do
            [Argo.value| null |] @?= Argo.Null
        , Tasty.testCase "Boolean" $ do
            [Argo.value| false |] @?= Argo.Boolean False
        , Tasty.testCase "Number" $ do
            [Argo.value| 0 |] @?= Argo.Number 0 0
        , Tasty.testCase "String" $ do
            [Argo.value| "" |] @?= Argo.String ""
        , Tasty.testCase "Array" $ do
            [Argo.value| [] |] @?= Argo.Array (array [])
        , Tasty.testCase "Object" $ do
            [Argo.value| {} |] @?= Argo.Object (array [])
        ]
    , Tasty.testGroup "property"
        [ property "decode . encode" $ \ x ->
            (resultToMaybe . Argo.decode . LazyByteString.toStrict . Builder.toLazyByteString $ Argo.encode x) === Just (x :: Argo.Value)
        , Tasty.testGroup "fromValue . toValue"
            [ property "Value" $ \ x ->
                (resultToMaybe . Argo.fromValue $ Argo.toValue x) === Just (x :: Argo.Value)
            , property "Bool" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Bool)
            , property "Char" $ \ x ->
                Argo.fromValue (Argo.toValue x) === if '\xd800' <= x && x <= '\xdfff'
                    then Argo.Success '\xfffd'
                    else Argo.Success x
            , property "Int" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Int)
            , property "Int8" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Int.Int8)
            , property "Int16" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Int.Int16)
            , property "Int32" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Int.Int32)
            , property "Int64" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Int.Int64)
            , property "Word" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Word)
            , property "Word8" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Word.Word8)
            , property "Word16" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Word.Word16)
            , property "Word32" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Word.Word32)
            , property "Word64" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Word.Word64)
            , property "Integer" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Integer)
            -- , property "Float" $ \ x ->
            --     Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Float)
            -- , property "Double" $ \ x ->
            --     Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Double)
            -- , property "String" $ \ x ->
            --     Argo.fromValue (Argo.toValue x) === Argo.Success (x :: String)
            , property "Text" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Text.Text)
            , property "LazyText" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: LazyText.Text)
            , property "Maybe a" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Maybe Bool)
            , property "()" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: ())
            , property "(a, b)" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: (Bool, Int.Int8))
            , property "Array Int a" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Array.Array Int Bool)
            , property "[a]" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: [Bool])
            , property "NonEmpty a" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: NonEmpty Bool)
            , property "Map Text a" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Argo.Success (x :: Map.Map Text.Text Bool)
            ]
        ]
    ]

property
    :: (Show t, Tasty.Testable prop, GenValidity.GenValid t)
    => Tasty.TestName
    -> (t -> prop)
    -> Tasty.TestTree
property n = propertyWith n GenValidity.genValid GenValidity.shrinkValid

propertyWith
    :: (Show t, Tasty.Testable prop)
    => Tasty.TestName
    -> Tasty.Gen t
    -> (t -> [t])
    -> (t -> prop)
    -> Tasty.TestTree
propertyWith n g s f = Tasty.testProperty n
    . Tasty.forAll g
    $ \ x -> Tasty.shrinking s x f

array :: [a] -> Array.Array Int a
array xs = Array.listArray (0, length xs - 1) xs

instance GenValidity.Validity (Argo.Pair String.String Argo.Value) where
    validate (Argo.Pair k v) = GenValidity.validate (k, v)

instance GenValidity.GenValid (Argo.Pair String.String Argo.Value) where
    genValid = Argo.Pair <$> GenValidity.genValid <*> GenValidity.genValid
    shrinkValid (Argo.Pair k v) = uncurry Argo.Pair <$> GenValidity.shrinkValid (k, v)

instance GenValidity.Validity Argo.Value where
    validate x = case x of
        Argo.Null -> GenValidity.valid
        Argo.Boolean y -> GenValidity.annotate y "Boolean"
        Argo.Number y z -> GenValidity.validate (y, z)
        Argo.String y -> GenValidity.annotate y "String"
        Argo.Array y -> GenValidity.annotate y "Array"
        Argo.Object y -> GenValidity.annotate y "Object"

instance GenValidity.GenValid Argo.Value where
    genValid = Tasty.sized genValueSized
    shrinkValid x = case x of
        Argo.Null -> []
        Argo.Boolean y -> Argo.Boolean <$> GenValidity.shrinkValid y
        Argo.Number y z -> uncurry Argo.Number <$> GenValidity.shrinkValid (y, z)
        Argo.String y -> Argo.String <$> GenValidity.shrinkValid y
        Argo.Array y -> Argo.Array <$> GenValidity.shrinkValid y
        Argo.Object y -> Argo.Object <$> GenValidity.shrinkValid y

genValueSized :: Int -> Tasty.Gen Argo.Value
genValueSized size = let newSize = div size 3 in Tasty.oneof
    [ pure Argo.Null
    , Argo.Boolean <$> GenValidity.genValid
    , Argo.Number <$> GenValidity.genValid <*> GenValidity.genValid
    , Argo.String <$> GenValidity.genValid
    , Argo.Array <$> genArray size (genValueSized newSize)
    , Argo.Object <$> genArray size (Argo.Pair <$> GenValidity.genValid <*> genValueSized newSize)
    ]

genArray :: Int -> Tasty.Gen a -> Tasty.Gen (Array.Array Int a)
genArray n = fmap (Array.listArray (0, n - 1)) . Tasty.vectorOf n

resultToMaybe :: Argo.Result a -> Maybe a
resultToMaybe r = case r of
    Argo.Failure _ -> Nothing
    Argo.Success x -> Just x

instance GenValidity.Validity a => GenValidity.Validity (Array.Array Int a) where
    validate = GenValidity.validate . Array.elems

instance GenValidity.GenValid a => GenValidity.GenValid (Array.Array Int a) where
    genValid = array <$> GenValidity.genValid
    shrinkValid = fmap array . GenValidity.shrinkValid . Array.elems
