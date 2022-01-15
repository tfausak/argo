{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Argo.Orphanage ()
import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.QuickCheck ((===))

import qualified Argo
import qualified Argo.Codec.Codec as Codec
import qualified Argo.Codec.Object as Codec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Json.Array as Array
import qualified Argo.Json.Boolean as Boolean
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Null as Null
import qualified Argo.Json.Number as Number
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Schema.Schema as Schema
import qualified Argo.Type.Decimal as Decimal
import qualified Argo.Type.Permission as Permission
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Word as Word
import qualified Numeric.Natural as Natural
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import qualified Test.Tasty.QuickCheck as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup
    "Argo"
    [ Tasty.testGroup "encode"
        $ let
              encode =
                  Builder.toLazyByteString . Argo.encode :: Argo.Value
                      -> LazyByteString.ByteString
          in
              [ Tasty.testGroup
                  "Null"
                  [ Tasty.testCase "null" $ do
                        encode Argo.Null @?= "null"
                  ]
              , Tasty.testGroup
                  "Boolean"
                  [ Tasty.testCase "false" $ do
                      encode (Argo.Boolean False) @?= "false"
                  , Tasty.testCase "true" $ do
                      encode (Argo.Boolean True) @?= "true"
                  ]
              , Tasty.testGroup
                  "Number"
                  [ Tasty.testCase "zero" $ do
                      encode (number 0 0) @?= "0"
                  , Tasty.testCase "positive integer" $ do
                      encode (number 1 0) @?= "1"
                  , Tasty.testCase "negative integer" $ do
                      encode (number (-1) 0) @?= "-1"
                  , Tasty.testCase "positive exponent" $ do
                      encode (number 1 1) @?= "1e1"
                  , Tasty.testCase "negative exponent" $ do
                      encode (number 1 (-1)) @?= "1e-1"
                  , Tasty.testCase "multiple integer digits" $ do
                      encode (number 12 0) @?= "12"
                  , Tasty.testCase "multiple exponent digits" $ do
                      encode (number 1 12) @?= "1e12"
                  ]
              , Tasty.testGroup
                  "String"
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
              , Tasty.testGroup
                  "Array"
                  [ Tasty.testCase "empty" $ do
                      encode (Argo.Array []) @?= "[]"
                  , Tasty.testCase "one element" $ do
                      encode (Argo.Array [number 1 0]) @?= "[1]"
                  , Tasty.testCase "two elements" $ do
                      encode (Argo.Array [number 1 0, number 2 0]) @?= "[1,2]"
                  ]
              , Tasty.testGroup
                  "Object"
                  [ Tasty.testCase "empty" $ do
                      encode (Argo.Object []) @?= "{}"
                  , Tasty.testCase "one element" $ do
                      encode
                              (Argo.Object
                                  [Argo.Member (Argo.Name "a") $ number 1 0]
                              )
                          @?= "{\"a\":1}"
                  , Tasty.testCase "two elements" $ do
                      encode
                              (Argo.Object
                                  [ Argo.Member (Argo.Name "a") $ number 1 0
                                  , Argo.Member (Argo.Name "b") $ number 2 0
                                  ]
                              )
                          @?= "{\"a\":1,\"b\":2}"
                  ]
              ]
    , Tasty.testGroup "decode"
        $ let
              decode =
                  hush . Argo.decode :: ByteString.ByteString
                      -> Maybe Argo.Value
          in
              [ Tasty.testGroup
                  "Null"
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
              , Tasty.testGroup
                  "Boolean"
                  [ Tasty.testCase "false" $ do
                      decode "false" @?= Just (Argo.Boolean False)
                  , Tasty.testCase "true" $ do
                      decode "true" @?= Just (Argo.Boolean True)
                  ]
              , Tasty.testGroup
                  "Number"
                  [ Tasty.testCase "zero" $ do
                      decode "0" @?= Just (number 0 0)
                  , Tasty.testCase "negative zero" $ do
                      decode "-0" @?= Just (number 0 0)
                  , Tasty.testCase "positive integer" $ do
                      decode "1" @?= Just (number 1 0)
                  , Tasty.testCase "multiple integer digits" $ do
                      decode "12" @?= Just (number 12 0)
                  , Tasty.testCase "negative integer" $ do
                      decode "-1" @?= Just (number (-1) 0)
                  , Tasty.testCase "fraction" $ do
                      decode "0.1" @?= Just (number 1 (-1))
                  , Tasty.testCase "multiple fraction digits" $ do
                      decode "0.12" @?= Just (number 12 (-2))
                  , Tasty.testCase "leading zero fraction" $ do
                      decode "0.01" @?= Just (number 1 (-2))
                  , Tasty.testCase "positive exponent" $ do
                      decode "1e1" @?= Just (number 1 1)
                  , Tasty.testCase "capital exponent" $ do
                      decode "1E1" @?= Just (number 1 1)
                  , Tasty.testCase "leading zero exponent" $ do
                      decode "1e01" @?= Just (number 1 1)
                  , Tasty.testCase "explicit positive exponent" $ do
                      decode "1e+1" @?= Just (number 1 1)
                  , Tasty.testCase "negative exponent" $ do
                      decode "1e-1" @?= Just (number 1 (-1))
                  , Tasty.testCase "multiple exponent digits" $ do
                      decode "1e12" @?= Just (number 1 12)
                  , Tasty.testCase "kitchen sink" $ do
                      decode "12.34e56" @?= Just (number 1234 54)
                  , Tasty.testCase "normalized" $ do
                      decode "10" @?= Just (number 1 1)
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
              , Tasty.testGroup
                  "String"
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
                      decode "\"\xf0\x90\x8d\x88\""
                          @?= Just (Argo.String "\x10348")
                  , Tasty.testCase "surrogate pair" $ do
                      decode "\"\\ud834\\udd1e\""
                          @?= Just (Argo.String "\x1d11e")
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
              , Tasty.testGroup
                  "Array"
                  [ Tasty.testCase "empty" $ do
                      decode "[]" @?= Just (Argo.Array [])
                  , Tasty.testCase "one element" $ do
                      decode "[1]" @?= Just (Argo.Array [number 1 0])
                  , Tasty.testCase "two elements" $ do
                      decode "[1,2]"
                          @?= Just (Argo.Array [number 1 0, number 2 0])
                  , Tasty.testCase "nested" $ do
                      decode "[1,[2]]"
                          @?= Just
                                  (Argo.Array
                                      [number 1 0, Argo.Array [number 2 0]]
                                  )
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
              , Tasty.testGroup
                  "Object"
                  [ Tasty.testCase "empty" $ do
                      decode "{}" @?= Just (Argo.Object [])
                  , Tasty.testCase "one element" $ do
                      decode "{\"a\":1}"
                          @?= Just
                                  (Argo.Object
                                      [ Argo.Member (Argo.Name "a")
                                            $ number 1 0
                                      ]
                                  )
                  , Tasty.testCase "two elements" $ do
                      decode "{\"a\":1,\"b\":2}" @?= Just
                          (Argo.Object
                              [ Argo.Member (Argo.Name "a") $ number 1 0
                              , Argo.Member (Argo.Name "b") $ number 2 0
                              ]
                          )
                  , Tasty.testCase "nested" $ do
                      decode "{\"a\":{\"b\":2}}" @?= Just
                          (Argo.Object
                              [ Argo.Member (Argo.Name "a") $ Argo.Object
                                    [Argo.Member (Argo.Name "b") $ number 2 0]
                              ]
                          )
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
    , Tasty.testGroup
        "fromValue"
        [ Tasty.testCase "Value" $ do
            Argo.fromValue Argo.Null @?= Right Argo.Null
        , Tasty.testCase "Bool" $ do
            Argo.fromValue (Argo.Boolean False) @?= Right False
        , Tasty.testCase "Char" $ do
            Argo.fromValue (Argo.String "a") @?= Right 'a'
        , Tasty.testCase "Int" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Int)
        , Tasty.testCase "Int8" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Int.Int8)
        , Tasty.testCase "Int16" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Int.Int16)
        , Tasty.testCase "Int32" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Int.Int32)
        , Tasty.testCase "Int64" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Int.Int64)
        , Tasty.testCase "Word" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Word)
        , Tasty.testCase "Word8" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Word.Word8)
        , Tasty.testCase "Word16" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Word.Word16)
        , Tasty.testCase "Word32" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Word.Word32)
        , Tasty.testCase "Word64" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Word.Word64)
        , Tasty.testCase "Integer" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Integer)
        , Tasty.testCase "Float" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Float)
        , Tasty.testCase "Double" $ do
            Argo.fromValue (number 0 0) @?= Right (0 :: Double)
        , Tasty.testCase "String" $ do
            Argo.fromValue (Argo.String "") @?= Right ("" :: String)
        , Tasty.testCase "Text" $ do
            Argo.fromValue (Argo.String "") @?= Right ("" :: Text.Text)
        , Tasty.testCase "LazyText" $ do
            Argo.fromValue (Argo.String "") @?= Right ("" :: LazyText.Text)
        , Tasty.testCase "Maybe a" $ do
            Argo.fromValue (Argo.Boolean False) @?= Right (Just False)
        , Tasty.testCase "()" $ do
            Argo.fromValue (Argo.Array []) @?= Right ()
        , Tasty.testCase "(a, b)" $ do
            Argo.fromValue (Argo.Array [Argo.Boolean False, Argo.String "a"])
                @?= Right (False, 'a')
        , Tasty.testCase "[a]" $ do
            Argo.fromValue (Argo.Array []) @?= Right ([] :: [Bool])
        , Tasty.testCase "NonEmpty a" $ do
            Argo.fromValue (Argo.Array [Argo.Boolean False])
                @?= Right (False :| [])
        , Tasty.testCase "Map Name a" $ do
            Argo.fromValue
                    (Argo.Object
                        [Argo.Member (Argo.Name "a") $ Argo.Boolean False]
                    )
                @?= Right (Map.fromList [(Argo.Name "a", False)])
        , Tasty.testCase "Pointer" $ do
            Argo.fromValue (Argo.String "") @?= Right (Argo.Pointer [])
        ]
    , Tasty.testGroup
        "toValue"
        [ Tasty.testCase "Value" $ do
            Argo.toValue Argo.Null @?= Argo.Null
        , Tasty.testCase "Bool" $ do
            Argo.toValue False @?= Argo.Boolean False
        , Tasty.testCase "Char" $ do
            Argo.toValue 'a' @?= Argo.String "a"
        , Tasty.testCase "Int" $ do
            Argo.toValue (0 :: Int) @?= number 0 0
        , Tasty.testCase "Int8" $ do
            Argo.toValue (0 :: Int.Int8) @?= number 0 0
        , Tasty.testCase "Int16" $ do
            Argo.toValue (0 :: Int.Int16) @?= number 0 0
        , Tasty.testCase "Int32" $ do
            Argo.toValue (0 :: Int.Int32) @?= number 0 0
        , Tasty.testCase "Int64" $ do
            Argo.toValue (0 :: Int.Int64) @?= number 0 0
        , Tasty.testCase "Word" $ do
            Argo.toValue (0 :: Word) @?= number 0 0
        , Tasty.testCase "Word8" $ do
            Argo.toValue (0 :: Word.Word8) @?= number 0 0
        , Tasty.testCase "Word16" $ do
            Argo.toValue (0 :: Word.Word16) @?= number 0 0
        , Tasty.testCase "Word32" $ do
            Argo.toValue (0 :: Word.Word32) @?= number 0 0
        , Tasty.testCase "Word64" $ do
            Argo.toValue (0 :: Word.Word64) @?= number 0 0
        , Tasty.testCase "Integer" $ do
            Argo.toValue (0 :: Integer) @?= number 0 0
        , Tasty.testCase "Float" $ do
            Argo.toValue (0 :: Float) @?= number 0 0
        , Tasty.testCase "Double" $ do
            Argo.toValue (0 :: Double) @?= number 0 0
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
            Argo.toValue (False, 'a')
                @?= Argo.Array [Argo.Boolean False, Argo.String "a"]
        , Tasty.testCase "[a]" $ do
            Argo.toValue ([] :: [Bool]) @?= Argo.Array []
        , Tasty.testCase "NonEmpty a" $ do
            Argo.toValue (False :| []) @?= Argo.Array [Argo.Boolean False]
        , Tasty.testCase "Map Name a" $ do
            Argo.toValue (Map.fromList [(Argo.Name "a", False)])
                @?= Argo.Object
                        [Argo.Member (Argo.Name "a") $ Argo.Boolean False]
        , Tasty.testCase "Pointer" $ do
            Argo.toValue (Argo.Pointer []) @?= Argo.String ""
        ]
    , Tasty.testGroup
        "quasi quoter"
        [ Tasty.testCase "Null" $ do
            [Argo.value| null |] @?= Argo.Null
        , Tasty.testCase "Boolean" $ do
            [Argo.value| false |] @?= Argo.Boolean False
        , Tasty.testCase "Number" $ do
            [Argo.value| 0 |] @?= number 0 0
        , Tasty.testCase "String" $ do
            [Argo.value| "" |] @?= Argo.String ""
        , Tasty.testCase "Array" $ do
            [Argo.value| [] |] @?= Argo.Array []
        , Tasty.testCase "Object" $ do
            [Argo.value| {} |] @?= Argo.Object []
        ]
    , Tasty.testGroup
        "property"
        [ property "decode . encode" $ \x ->
            (Argo.decode
                . LazyByteString.toStrict
                . Builder.toLazyByteString
                $ Argo.encode x
                )
                === Right (x :: Argo.Value)
        , property "decode . encodeWith" $ \x ->
            (Argo.decode
                . LazyByteString.toStrict
                . Builder.toLazyByteString
                $ Argo.encodeWith Argo.Tab x
                )
                === Right (x :: Argo.Value)
        , Tasty.testGroup
            "fromValue . toValue"
            [ property "Value" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Argo.Value)
            , property "Bool" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Bool)
            , property "Char" $ \x ->
                Argo.fromValue (Argo.toValue x)
                    === if '\xd800' <= x && x <= '\xdfff'
                            then Right '\xfffd'
                            else Right x
            , property "Int" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int)
            , property "Int8" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int.Int8)
            , property "Int16" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int.Int16)
            , property "Int32" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int.Int32)
            , property "Int64" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Int.Int64)
            , property "Word" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Word)
            , property "Word8" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Word.Word8)
            , property "Word16"
                $ \x -> Argo.fromValue (Argo.toValue x)
                      === Right (x :: Word.Word16)
            , property "Word32" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Word.Word32)
            , property "Word64"
                $ \x -> Argo.fromValue (Argo.toValue x)
                      === Right (x :: Word.Word64)
            , property "Integer" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Integer)
            , property "Float" $ \x ->
                Argo.fromValue (Argo.toValue x) === if isNaN x || isInfinite x
                    then Left "expected Float but got Null (Null ())"
                    else Right (x :: Float)
            , property "Double" $ \x ->
                Argo.fromValue (Argo.toValue x) === if isNaN x || isInfinite x
                    then Left "expected Double but got Null (Null ())"
                    else Right (x :: Double)
            , property "String"
                $ \x -> Argo.fromValue (Argo.toValue x)
                      === Right (Text.unpack $ Text.pack x)
            , property "Text" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Text.Text)
            , property "LazyText"
                $ \x -> Argo.fromValue (Argo.toValue x)
                      === Right (x :: LazyText.Text)
            , property "Maybe a" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Maybe Bool)
            , property "()"
                $ \x -> Argo.fromValue (Argo.toValue x) === Right (x :: ())
            , property "(a, b)"
                $ \x -> Argo.fromValue (Argo.toValue x)
                      === Right (x :: (Bool, Int.Int8))
            , property "[a]"
                $ \x -> Argo.fromValue (Argo.toValue x) === Right (x :: [Bool])
            , property "NonEmpty a"
                $ \x -> Argo.fromValue (Argo.toValue x)
                      === Right (x :: NonEmpty Bool)
            , property "Map Name a"
                $ \x -> Argo.fromValue (Argo.toValue x)
                      === Right (x :: Map.Map Argo.Name Bool)
            , property "Pointer" $ \x ->
                Argo.fromValue (Argo.toValue x) === Right (x :: Argo.Pointer)
            ]
        ]
    , Tasty.testGroup
        "Codec"
        [ Tasty.testCase "encode text" $ do
            Codec.encodeWith Argo.codec ("" :: Text.Text) @?= Argo.String ""
        , Tasty.testCase "decode text" $ do
            Codec.decodeWith Argo.codec (Argo.String "")
                @?= Right ("" :: Text.Text)
        , Tasty.testCase "encode bool" $ do
            Codec.encodeWith Argo.codec False @?= Argo.Boolean False
        , Tasty.testCase "decode bool" $ do
            Codec.decodeWith Argo.codec (Argo.Boolean False) @?= Right False
        , Tasty.testCase "encode maybe bool" $ do
            Codec.encodeWith Argo.codec (Nothing :: Maybe Bool) @?= Argo.Null
            Codec.encodeWith Argo.codec (Just False) @?= Argo.Boolean False
        , Tasty.testCase "decode maybe bool" $ do
            Codec.decodeWith Argo.codec Argo.Null
                @?= Right (Nothing :: Maybe Bool)
            Codec.decodeWith Argo.codec (Argo.Boolean False)
                @?= Right (Just False)
        , Tasty.testCase "encode either text bool" $ do
            Codec.encodeWith Argo.codec (Left "" :: Either Text.Text Bool)
                @?= Argo.Object
                        [ Argo.Member (Argo.Name "type") $ Argo.String "Left"
                        , Argo.Member (Argo.Name "value") $ Argo.String ""
                        ]
            Codec.encodeWith Argo.codec (Right False :: Either Text.Text Bool)
                @?= Argo.Object
                        [ Argo.Member (Argo.Name "type") $ Argo.String "Right"
                        , Argo.Member (Argo.Name "value") $ Argo.Boolean False
                        ]
        , Tasty.testCase "decode either text bool" $ do
            Codec.decodeWith
                    Argo.codec
                    (Argo.Object
                        [ Argo.Member (Argo.Name "type") $ Argo.String "Left"
                        , Argo.Member (Argo.Name "value") $ Argo.String ""
                        ]
                    )
                @?= Right (Left "" :: Either Text.Text Bool)
            Codec.decodeWith
                    Argo.codec
                    (Argo.Object
                        [ Argo.Member (Argo.Name "type") $ Argo.String "Right"
                        , Argo.Member (Argo.Name "value") $ Argo.Boolean False
                        ]
                    )
                @?= Right (Right False :: Either Text.Text Bool)
        , Tasty.testCase "encode tuple text bool" $ do
            Codec.encodeWith Argo.codec ("" :: Text.Text, False)
                @?= Argo.Array [Argo.String "", Argo.Boolean False]
        , Tasty.testCase "decode tuple text bool" $ do
            Codec.decodeWith
                    Argo.codec
                    (Argo.Array [Argo.String "", Argo.Boolean False])
                @?= Right ("" :: Text.Text, False)
        , Tasty.testCase "encode record" $ do
            Codec.encodeWith Argo.codec (Record False Nothing)
                @?= Argo.Object
                        [Argo.Member (Argo.Name "bool") $ Argo.Boolean False]
            Codec.encodeWith Argo.codec (Record False $ Just "")
                @?= Argo.Object
                        [ Argo.Member (Argo.Name "bool") $ Argo.Boolean False
                        , Argo.Member (Argo.Name "text") $ Argo.String ""
                        ]
        , Tasty.testCase "decode record" $ do
            Codec.decodeWith
                    Argo.codec
                    (Argo.Object
                        [Argo.Member (Argo.Name "bool") $ Argo.Boolean False]
                    )
                @?= Right (Record False Nothing)
            Codec.decodeWith
                    Argo.codec
                    (Argo.Object
                        [ Argo.Member (Argo.Name "bool") $ Argo.Boolean False
                        , Argo.Member (Argo.Name "text") $ Argo.String ""
                        ]
                    )
                @?= Right (Record False $ Just "")
        ]
    , Tasty.testGroup "Pointer"
        $ let pointer = Argo.Pointer . fmap Argo.Token
          in
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
              , property "decode . encode" $ \x ->
                  (Argo.decodePointer
                      . LazyByteString.toStrict
                      . Builder.toLazyByteString
                      $ Argo.encodePointer x
                      )
                      === Right (x :: Argo.Pointer)
              , Tasty.testCase "quasi-quoter" $ do
                  [Argo.pointer||] @?= pointer []
                  [Argo.pointer|/|] @?= pointer [""]
                  [Argo.pointer|/a|] @?= pointer ["a"]
                  [Argo.pointer|/a/b|] @?= pointer ["a", "b"]
              , Tasty.testCase "evaluate" $ do
                  let evaluate p = hush . Argo.evaluate p
                  evaluate (pointer []) Argo.Null @?= Just Argo.Null
                  evaluate
                          (pointer ["a"])
                          (Argo.Object [Argo.Member (Argo.Name "a") Argo.Null])
                      @?= Just Argo.Null
                  evaluate
                          (pointer ["a", "b"])
                          (Argo.Object
                              [ Argo.Member (Argo.Name "a") $ Argo.Object
                                    [Argo.Member (Argo.Name "b") Argo.Null]
                              ]
                          )
                      @?= Just Argo.Null
                  evaluate
                          (pointer ["0"])
                          (Argo.Object [Argo.Member (Argo.Name "0") Argo.Null])
                      @?= Just Argo.Null
                  evaluate (pointer ["0"]) (Argo.Array [Argo.Null])
                      @?= Just Argo.Null
                  evaluate
                          (pointer ["0", "1"])
                          (Argo.Array
                              [Argo.Array [Argo.Boolean False, Argo.Null]]
                          )
                      @?= Just Argo.Null
                  evaluate (pointer ["a"]) Argo.Null @?= Nothing
                  evaluate (pointer ["a"]) (Argo.Object []) @?= Nothing
                  evaluate (pointer ["a"]) (Argo.Array [Argo.Null]) @?= Nothing
                  evaluate (pointer ["0"]) (Argo.Array []) @?= Nothing
                  evaluate (pointer ["-"]) (Argo.Array []) @?= Nothing
                  evaluate (pointer ["00"]) (Argo.Array [Argo.Null])
                      @?= Nothing
              ]
    , Tasty.testGroup
        "OverloadedStrings"
        [ Tasty.testCase "Value" $ do
            "" @?= Argo.String ""
        , Tasty.testCase "String" $ do
            "" @?= String.String ""
        , Tasty.testCase "Name" $ do
            "" @?= Argo.Name ""
        , Tasty.testCase "Token" $ do
            "" @?= Argo.Token ""
        ]
    , Tasty.testGroup
        "Schema"
        [ Tasty.testCase "value" $ do
            let expected = Schema.fromValue [Argo.value| true |]
                actual = Codec.schema (Argo.codec :: Codec.Value Argo.Value)
            actual @?= expected
        , Tasty.testCase "null" $ do
            let expected = Schema.fromValue [Argo.value| { "type": "null" } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Null.Null)
            actual @?= expected
        , Tasty.testCase "boolean" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "boolean" } |]
                actual =
                    Codec.schema (Argo.codec :: Codec.Value Boolean.Boolean)
            actual @?= expected
        , Tasty.testCase "number" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "number" } |]
                actual =
                    Codec.schema (Argo.codec :: Codec.Value Number.Number)
            actual @?= expected
        , Tasty.testCase "string" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "string" } |]
                actual =
                    Codec.schema (Argo.codec :: Codec.Value String.String)
            actual @?= expected
        , Tasty.testCase "array boolean" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "array", "items": { "type": "boolean" } } |]
                actual =
                    Codec.schema
                        (Argo.codec :: Codec.Value
                              (Array.Array Boolean.Boolean)
                        )
            actual @?= expected
        , Tasty.testCase "object boolean" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "object", "additionalProperties": { "type": "boolean" } } |]
                actual =
                    Codec.schema
                        (Argo.codec :: Codec.Value
                              (Object.Object Boolean.Boolean)
                        )
            actual @?= expected
        , Tasty.testCase "maybe boolean" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "oneOf": [ { "type": "boolean" }, { "type": "null" } ] } |]
                actual = Codec.schema
                    (Argo.codec :: Codec.Value (Maybe Boolean.Boolean))
            actual @?= expected
        , Tasty.testCase "either boolean number" $ pure () -- TODO
        , Tasty.testCase "()" $ pure () -- TODO
        , Tasty.testCase "(boolean, number)" $ pure () -- TODO
        , Tasty.testCase "bool" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "boolean" } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Bool)
            actual @?= expected
        , Tasty.testCase "decimal" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "number" } |]
                actual =
                    Codec.schema (Argo.codec :: Codec.Value Decimal.Decimal)
            actual @?= expected
        , Tasty.testCase "text" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "string" } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Text.Text)
            actual @?= expected
        , Tasty.testCase "list boolean" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "array", "items": { "type": "boolean" } } |]
                actual =
                    Codec.schema (Argo.codec :: Codec.Value [Boolean.Boolean])
            actual @?= expected
        , Tasty.testCase "map name boolean" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "object", "additionalProperties": { "type": "boolean" } } |]
                actual =
                    Codec.schema
                        (Argo.codec :: Codec.Value
                              (Map.Map Name.Name Boolean.Boolean)
                        )
            actual @?= expected
        , Tasty.testCase "string" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "string" } |]
                actual = Codec.schema (Argo.codec :: Codec.Value String)
            actual @?= expected
        , Tasty.testCase "char" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "string", "minLength": 1, "maxLength": 1 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Char)
            actual @?= expected
        , Tasty.testCase "lazy text" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "string" } |]
                actual =
                    Codec.schema (Argo.codec :: Codec.Value LazyText.Text)
            actual @?= expected
        , Tasty.testCase "list boolean" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "array", "items": { "type": "boolean" }, "minItems": 1 } |]
                actual = Codec.schema
                    (Argo.codec :: Codec.Value (NonEmpty Boolean.Boolean))
            actual @?= expected
        , Tasty.testCase "integer" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "integer" } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Integer)
            actual @?= expected
        , Tasty.testCase "int" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": -9223372036854775808, "maximum": 9223372036854775807 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Int)
            actual @?= expected
        , Tasty.testCase "int8" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": -128, "maximum": 127 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Int.Int8)
            actual @?= expected
        , Tasty.testCase "int16" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": -32768, "maximum": 32767 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Int.Int16)
            actual @?= expected
        , Tasty.testCase "int32" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Int.Int32)
            actual @?= expected
        , Tasty.testCase "int64" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": -9223372036854775808, "maximum": 9223372036854775807 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Int.Int64)
            actual @?= expected
        , Tasty.testCase "natural" $ do
            let expected = Schema.fromValue
                    [Argo.value| { "type": "integer", "minimum": 0 } |]
                actual =
                    Codec.schema (Argo.codec :: Codec.Value Natural.Natural)
            actual @?= expected
        , Tasty.testCase "word" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": 0, "maximum": 18446744073709551615 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Word)
            actual @?= expected
        , Tasty.testCase "word8" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": 0, "maximum": 255 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Word.Word8)
            actual @?= expected
        , Tasty.testCase "word16" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": 0, "maximum": 65535 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Word.Word16)
            actual @?= expected
        , Tasty.testCase "word32" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": 0, "maximum": 4294967295 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Word.Word32)
            actual @?= expected
        , Tasty.testCase "word64" $ do
            let expected =
                    Schema.fromValue
                        [Argo.value| { "type": "integer", "minimum": 0, "maximum": 18446744073709551615 } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Word.Word64)
            actual @?= expected
        , Tasty.testCase "float" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "number" } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Float)
            actual @?= expected
        , Tasty.testCase "double" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "number" } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Double)
            actual @?= expected
        , Tasty.testCase "pointer" $ do
            let expected =
                    Schema.fromValue [Argo.value| { "type": "string" } |]
                actual = Codec.schema (Argo.codec :: Codec.Value Argo.Pointer)
            actual @?= expected
        , Tasty.testCase "schema" $ do
            let expected = Schema.fromValue [Argo.value| true |]
                actual =
                    Codec.schema (Argo.codec :: Codec.Value Schema.Schema)
            actual @?= expected
        ]
    ]

number :: Integer -> Integer -> Argo.Value
number s = Argo.Number . Argo.Decimal s

data Record = Record
    { recordBool :: Bool
    , recordText :: Maybe Text.Text
    }
    deriving (Eq, Show)

instance Argo.HasCodec Record where
    codec =
        Codec.fromObjectCodec Permission.Allow
            $ Record
            <$> Codec.project
                    recordBool
                    (Codec.required (Argo.Name "bool") Argo.codec)
            <*> Codec.project
                    recordText
                    (Codec.optional (Argo.Name "text") Argo.codec)

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
propertyWith n g s f =
    Tasty.testProperty n . Tasty.forAll g $ \x -> Tasty.shrinking s x f

hush :: Either String a -> Maybe a
hush = either (const Nothing) Just
