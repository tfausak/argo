{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.QuickCheck ((===))

import qualified Argo
import qualified Argo.Type.Codec as Codec
import qualified Argo.Type.Permission as Permission
import qualified Argo.Json.String as String
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
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
                encode (Argo.Array []) @?= "[]"
            , Tasty.testCase "one element" $ do
                encode (Argo.Array [Argo.Number 1 0]) @?= "[1]"
            , Tasty.testCase "two elements" $ do
                encode (Argo.Array [Argo.Number 1 0, Argo.Number 2 0]) @?= "[1,2]"
            ]
        , Tasty.testGroup "Object"
            [ Tasty.testCase "empty" $ do
                encode (Argo.Object []) @?= "{}"
            , Tasty.testCase "one element" $ do
                encode (Argo.Object [Argo.Member (Argo.Name "a") $ Argo.Number 1 0]) @?= "{\"a\":1}"
            , Tasty.testCase "two elements" $ do
                encode (Argo.Object [Argo.Member (Argo.Name "a") $ Argo.Number 1 0, Argo.Member (Argo.Name "b") $ Argo.Number 2 0]) @?= "{\"a\":1,\"b\":2}"
            ]
        ]
    , Tasty.testGroup "decode" $ let decode = hush . Argo.decode :: ByteString.ByteString -> Maybe Argo.Value in
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
                decode "[]" @?= Just (Argo.Array [])
            , Tasty.testCase "one element" $ do
                decode "[1]" @?= Just (Argo.Array [Argo.Number 1 0])
            , Tasty.testCase "two elements" $ do
                decode "[1,2]" @?= Just (Argo.Array [Argo.Number 1 0, Argo.Number 2 0])
            , Tasty.testCase "nested" $ do
                decode "[1,[2]]" @?= Just (Argo.Array [Argo.Number 1 0, Argo.Array [Argo.Number 2 0]])
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
                decode "{}" @?= Just (Argo.Object [])
            , Tasty.testCase "one element" $ do
                decode "{\"a\":1}" @?= Just (Argo.Object [Argo.Member (Argo.Name "a") $ Argo.Number 1 0])
            , Tasty.testCase "two elements" $ do
                decode "{\"a\":1,\"b\":2}" @?= Just (Argo.Object [Argo.Member (Argo.Name "a") $ Argo.Number 1 0, Argo.Member (Argo.Name "b") $ Argo.Number 2 0])
            , Tasty.testCase "nested" $ do
                decode "{\"a\":{\"b\":2}}" @?= Just (Argo.Object [Argo.Member (Argo.Name "a") $ Argo.Object [Argo.Member (Argo.Name "b") $ Argo.Number 2 0]])
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
            fromValue = hush . Argo.fromValue
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
            fromValue (Argo.Array []) @?= Just ()
        , Tasty.testCase "(a, b)" $ do
            fromValue (Argo.Array [Argo.Boolean False, Argo.String "a"]) @?= Just (False, 'a')
        , Tasty.testCase "[a]" $ do
            fromValue (Argo.Array []) @?= Just ([] :: [Bool])
        , Tasty.testCase "NonEmpty a" $ do
            fromValue (Argo.Array [Argo.Boolean False]) @?= Just (False :| [])
        , Tasty.testCase "Map Text a" $ do
            fromValue (Argo.Object [Argo.Member (Argo.Name "a") $ Argo.Boolean False]) @?= Just (Map.fromList [("a" :: Text.Text, False)])
        , Tasty.testCase "Pointer" $ do
            fromValue (Argo.String "") @?= Just (Argo.Pointer [])
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
            Argo.toValue () @?= Argo.Array []
        , Tasty.testCase "(a, b)" $ do
            Argo.toValue (False, 'a') @?= Argo.Array [Argo.Boolean False, Argo.String "a"]
        , Tasty.testCase "[a]" $ do
            Argo.toValue ([] :: [Bool]) @?= Argo.Array []
        , Tasty.testCase "NonEmpty a" $ do
            Argo.toValue (False :| []) @?= Argo.Array [Argo.Boolean False]
        , Tasty.testCase "Map Text a" $ do
            Argo.toValue (Map.fromList [("a" :: Text.Text, False)]) @?= Argo.Object [Argo.Member (Argo.Name "a") $ Argo.Boolean False]
        , Tasty.testCase "Pointer" $ do
            Argo.toValue (Argo.Pointer []) @?= Argo.String ""
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
            [Argo.value| [] |] @?= Argo.Array []
        , Tasty.testCase "Object" $ do
            [Argo.value| {} |] @?= Argo.Object []
        ]
    , Tasty.testGroup "property"
        [ property "decode . encode" $ \ x ->
            (Argo.decode . LazyByteString.toStrict . Builder.toLazyByteString $ Argo.encode x) === Right (x :: Argo.Value)
        , property "decode . encodeWith" $ \ x ->
            (Argo.decode . LazyByteString.toStrict . Builder.toLazyByteString $ Argo.encodeWith Argo.Tab x) === Right (x :: Argo.Value)
        , Tasty.testGroup "fromValue . toValue"
            [ property "Value" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Argo.Value)
            , property "Bool" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Bool)
            , property "Char" $ \ x ->
                Argo.fromValue (Argo.toValue x) === if '\xd800' <= x && x <= '\xdfff'
                    then Right '\xfffd'
                    else Right x
            , property "Int" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int)
            , property "Int8" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int.Int8)
            , property "Int16" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int.Int16)
            , property "Int32" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int.Int32)
            , property "Int64" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int.Int64)
            , property "Word" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Word)
            , property "Word8" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Word.Word8)
            , property "Word16" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Word.Word16)
            , property "Word32" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Word.Word32)
            , property "Word64" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Word.Word64)
            , property "Integer" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Integer)
            , property "Float" $ \ x ->
                Argo.fromValue (Argo.toValue x) === if isNaN x || isInfinite x
                    then Left "expected Float but got Null (Null ())"
                    else Right (x :: Float)
            , property "Double" $ \ x ->
                Argo.fromValue (Argo.toValue x) === if isNaN x || isInfinite x
                    then Left "expected Double but got Null (Null ())"
                    else Right (x :: Double)
            , property "String" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (Text.unpack $ Text.pack x)
            , property "Text" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Text.Text)
            , property "LazyText" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: LazyText.Text)
            , property "Maybe a" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Maybe Bool)
            , property "()" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: ())
            , property "(a, b)" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: (Bool, Int.Int8))
            , property "[a]" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: [Bool])
            , property "NonEmpty a" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: NonEmpty Bool)
            , property "Map Text a" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Map.Map Text.Text Bool)
            , property "Pointer" $ \ x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Argo.Pointer)
            ]
        ]
    , Tasty.testGroup "Codec"
        [ Tasty.testCase "encode text" $ do
            Codec.encodeWith Codec.textCodec "" @?= Argo.String ""
        , Tasty.testCase "decode text" $ do
            Codec.decodeWith Codec.textCodec (Argo.String "") @?= Right ""
        , Tasty.testCase "encode bool" $ do
            Codec.encodeWith Codec.boolCodec False @?= Argo.Boolean False
        , Tasty.testCase "decode bool" $ do
            Codec.decodeWith Codec.boolCodec (Argo.Boolean False) @?= Right False
        , Tasty.testCase "encode maybe text" $ do
            Codec.encodeWith (Codec.maybeCodec Codec.textCodec) Nothing @?= Argo.Null
            Codec.encodeWith (Codec.maybeCodec Codec.textCodec) (Just "") @?= Argo.String ""
        , Tasty.testCase "decode maybe text" $ do
            Codec.decodeWith (Codec.maybeCodec Codec.textCodec) Argo.Null @?= Right Nothing
            Codec.decodeWith (Codec.maybeCodec Codec.textCodec) (Argo.String "") @?= Right (Just "")
        , Tasty.testCase "encode either text bool" $ do
            Codec.encodeWith (Codec.eitherCodec Codec.textCodec Codec.boolCodec) (Left "") @?= Argo.Object [Argo.Member (Argo.Name "type") $ Argo.String "Left", Argo.Member (Argo.Name "value") $ Argo.String ""]
            Codec.encodeWith (Codec.eitherCodec Codec.textCodec Codec.boolCodec) (Right False) @?= Argo.Object [Argo.Member (Argo.Name "type") $ Argo.String "Right", Argo.Member (Argo.Name "value") $ Argo.Boolean False]
        , Tasty.testCase "decode either text bool" $ do
            Codec.decodeWith (Codec.eitherCodec Codec.textCodec Codec.boolCodec) (Argo.Object [Argo.Member (Argo.Name "type") $ Argo.String "Left", Argo.Member (Argo.Name "value") $ Argo.String ""]) @?= Right (Left "")
            Codec.decodeWith (Codec.eitherCodec Codec.textCodec Codec.boolCodec) (Argo.Object [Argo.Member (Argo.Name "type") $ Argo.String "Right", Argo.Member (Argo.Name "value") $ Argo.Boolean False]) @?= Right (Right False)
        , Tasty.testCase "encode tuple text bool" $ do
            Codec.encodeWith (Codec.tupleCodec Codec.textCodec Codec.boolCodec) ("", False) @?= Argo.Array [Argo.String "", Argo.Boolean False]
        , Tasty.testCase "decode tuple text bool" $ do
            Codec.decodeWith (Codec.tupleCodec Codec.textCodec Codec.boolCodec) (Argo.Array [Argo.String "", Argo.Boolean False]) @?= Right ("", False)
        , Tasty.testCase "encode record" $ do
            Codec.encodeWith recordCodec (Record False Nothing) @?= Argo.Object [Argo.Member (Argo.Name "bool") $ Argo.Boolean False]
            Codec.encodeWith recordCodec (Record False $ Just "") @?= Argo.Object [Argo.Member (Argo.Name "bool") $ Argo.Boolean False, Argo.Member (Argo.Name "text") $ Argo.String ""]
        , Tasty.testCase "decode record" $ do
            Codec.decodeWith recordCodec (Argo.Object [Argo.Member (Argo.Name "bool") $ Argo.Boolean False]) @?= Right (Record False Nothing)
            Codec.decodeWith recordCodec (Argo.Object [Argo.Member (Argo.Name "bool") $ Argo.Boolean False, Argo.Member (Argo.Name "text") $ Argo.String ""]) @?= Right (Record False $ Just "")
        ]
    , Tasty.testGroup "Pointer"
        $ let pointer = Argo.Pointer . fmap Argo.Token in
        [ Tasty.testCase "decode" $ do
            let decode = hush . Argo.decodePointer
            decode "" @?= Just (pointer [])
            decode "/" @?= Just (pointer [""])
            decode "/a" @?= Just (pointer ["a"])
            decode "/a/b" @?= Just (pointer ["a", "b"])
            decode "/ab" @?= Just (pointer ["ab"])
            decode "/~0" @?= Just (pointer ["~"])
            decode "/~1" @?= Just (pointer ["/"])
            decode "/~01" @?= Just (pointer ["~1"])
            decode "a" @?= Nothing
            decode "/~2" @?= Nothing
        , Tasty.testCase "encode" $ do
            let encode = Builder.toLazyByteString . Argo.encodePointer
            encode (pointer []) @?= ""
            encode (pointer [""]) @?= "/"
            encode (pointer ["a"]) @?= "/a"
            encode (pointer ["a", "b"]) @?= "/a/b"
            encode (pointer ["ab"]) @?= "/ab"
            encode (pointer ["~"]) @?= "/~0"
            encode (pointer ["/"]) @?= "/~1"
            encode (pointer ["~1"]) @?= "/~01"
        , property "decode . encode" $ \ x ->
            (Argo.decodePointer . LazyByteString.toStrict . Builder.toLazyByteString $ Argo.encodePointer x) === Right (x :: Argo.Pointer)
        , Tasty.testCase "quasi-quoter" $ do
            [Argo.pointer||] @?= pointer []
            [Argo.pointer|/|] @?= pointer [""]
            [Argo.pointer|/a|] @?= pointer ["a"]
            [Argo.pointer|/a/b|] @?= pointer ["a", "b"]
        , Tasty.testCase "evaluate" $ do
            let evaluate p = hush . Argo.evaluate p
            evaluate (pointer []) Argo.Null @?= Just Argo.Null
            evaluate (pointer ["a"]) (Argo.Object [Argo.Member (Argo.Name "a") Argo.Null]) @?= Just Argo.Null
            evaluate (pointer ["a", "b"]) (Argo.Object [Argo.Member (Argo.Name "a") $ Argo.Object [Argo.Member (Argo.Name "b") Argo.Null]]) @?= Just Argo.Null
            evaluate (pointer ["0"]) (Argo.Object [Argo.Member (Argo.Name "0") Argo.Null]) @?= Just Argo.Null
            evaluate (pointer ["0"]) (Argo.Array [Argo.Null]) @?= Just Argo.Null
            evaluate (pointer ["0", "1"]) (Argo.Array [Argo.Array [Argo.Boolean False, Argo.Null]]) @?= Just Argo.Null
            evaluate (pointer ["a"]) Argo.Null @?= Nothing
            evaluate (pointer ["a"]) (Argo.Object []) @?= Nothing
            evaluate (pointer ["a"]) (Argo.Array [Argo.Null]) @?= Nothing
            evaluate (pointer ["0"]) (Argo.Array []) @?= Nothing
            evaluate (pointer ["-"]) (Argo.Array []) @?= Nothing
            evaluate (pointer ["00"]) (Argo.Array [Argo.Null]) @?= Nothing
        ]
    , Tasty.testGroup "OverloadedStrings"
        [ Tasty.testCase "Value" $ do
            "" @?= Argo.String ""
        , Tasty.testCase "String" $ do
            "" @?= String.String ""
        , Tasty.testCase "Name" $ do
            "" @?= Argo.Name ""
        , Tasty.testCase "Token" $ do
            "" @?= Argo.Token ""
        ]
    ]

data Record = Record
    { recordBool :: Bool
    , recordText :: Maybe Text.Text
    } deriving (Eq, Show)

recordCodec :: Codec.ValueCodec Record
recordCodec = Codec.fromObjectCodec Permission.Allow $ Record
    <$> Codec.project recordBool (Codec.required (Argo.Name "bool") Codec.boolCodec)
    <*> Codec.project recordText (Codec.optional (Argo.Name "text") Codec.textCodec)

property
    :: (Show t, Tasty.Testable prop, Tasty.Arbitrary t)
    => Tasty.TestName
    -> (t -> prop)
    -> Tasty.TestTree
property n = propertyWith n Tasty.arbitrary Tasty.shrink

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

instance Tasty.Arbitrary Argo.Name where
    arbitrary = Argo.Name <$> Tasty.arbitrary
    shrink (Argo.Name x) = Argo.Name <$> Tasty.shrink x

instance Tasty.Arbitrary value => Tasty.Arbitrary (Argo.MemberOf value) where
    arbitrary = Argo.Member <$> Tasty.arbitrary <*> Tasty.arbitrary
    shrink (Argo.Member k v) = uncurry Argo.Member <$> Tasty.shrink (k, v)

instance Tasty.Arbitrary Argo.Value where
    arbitrary = Tasty.sized genValueSized
    shrink x = case x of
        Argo.Null -> []
        Argo.Boolean y -> Argo.Boolean <$> Tasty.shrink y
        Argo.Number y z -> uncurry Argo.Number <$> Tasty.shrink (y, z)
        Argo.String y -> Argo.String <$> Tasty.shrink y
        Argo.Array y -> Argo.Array <$> Tasty.shrink y
        Argo.Object y -> Argo.Object <$> Tasty.shrink y

genValueSized :: Int -> Tasty.Gen Argo.Value
genValueSized size = let newSize = div size 3 in Tasty.oneof
    [ pure Argo.Null
    , Argo.Boolean <$> Tasty.arbitrary
    , Argo.Number <$> Tasty.arbitrary <*> Tasty.arbitrary
    , Argo.String <$> Tasty.arbitrary
    , Argo.Array <$> Tasty.vectorOf size (genValueSized newSize)
    , Argo.Object <$> Tasty.vectorOf size (Argo.Member <$> Tasty.arbitrary <*> genValueSized newSize)
    ]

hush :: Either String a -> Maybe a
hush = either (const Nothing) Just

instance Tasty.Arbitrary Text.Text where
    arbitrary = Text.pack <$> Tasty.arbitrary
    shrink = Tasty.shrinkMap Text.pack Text.unpack

instance Tasty.Arbitrary LazyText.Text where
    arbitrary = LazyText.pack <$> Tasty.arbitrary
    shrink = Tasty.shrinkMap LazyText.pack LazyText.unpack

instance Tasty.Arbitrary a => Tasty.Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> Tasty.arbitrary <*> Tasty.arbitrary
    shrink (x :| xs) = uncurry (:|) <$> Tasty.shrink (x, xs)

instance Tasty.Arbitrary Argo.Pointer where
    arbitrary = Argo.Pointer <$> Tasty.arbitrary
    shrink (Argo.Pointer x) = Argo.Pointer <$> Tasty.shrink x

instance Tasty.Arbitrary Argo.Token where
    arbitrary = Argo.Token <$> Tasty.arbitrary
    shrink (Argo.Token x) = Argo.Token <$> Tasty.shrink x
