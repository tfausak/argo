{-# LANGUAGE DeriveLift #-}

module Argo.Internal.Pointer.Pointer where

import qualified Argo.Internal.Json.Array as Array
import qualified Argo.Internal.Json.Member as Member
import qualified Argo.Internal.Json.Name as Name
import qualified Argo.Internal.Json.Object as Object
import qualified Argo.Internal.Json.String as String
import qualified Argo.Internal.Json.Value as Value
import qualified Argo.Internal.Literal as Literal
import qualified Argo.Internal.Pointer.Token as Token
import qualified Argo.Internal.Type.Decoder as Decoder
import qualified Argo.Internal.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Applicative as Applicative
import qualified Data.List as List
import qualified Text.Read as Read

-- | A JSON pointer, as described by RFC 6901.
-- <https://datatracker.ietf.org/doc/html/rfc6901>
newtype Pointer
    = Pointer [Token.Token]
    deriving (Eq, TH.Lift, Show)

instance DeepSeq.NFData Pointer where
    rnf = DeepSeq.rnf . toList

fromList :: [Token.Token] -> Pointer
fromList = Pointer

toList :: Pointer -> [Token.Token]
toList (Pointer x) = x

decode :: Decoder.Decoder Pointer
decode = fromList <$> Applicative.many decodeToken

decodeToken :: Decoder.Decoder Token.Token
decodeToken = do
    Decoder.word8 Literal.solidus
    Token.decode

encode :: Pointer -> Encoder.Encoder ()
encode = mapM_ encodeToken . toList

encodeToken :: Token.Token -> Encoder.Encoder ()
encodeToken x = do
    Trans.lift . Trans.tell $ Builder.word8 Literal.solidus
    Token.encode x

evaluate :: Pointer -> Value.Value -> Either String Value.Value
evaluate p v = case toList p of
    [] -> pure v
    t : ts -> do
        w <- case v of
            Value.Array a -> atIndex t a
            Value.Object o -> atKey t o
            _ -> Left "not indexable"
        evaluate (fromList ts) w

atIndex :: Token.Token -> Array.Array value -> Either String value
atIndex t a = do
    i <- tokenToIndex t
    case drop i $ Array.toList a of
        [] -> Left $ "missing index: " <> show t
        e : _ -> pure e

tokenToIndex :: Token.Token -> Either String Int
tokenToIndex token = do
    let text = Token.toText token
        invalid = "invalid index: " <> show token
    case Text.uncons text of
        Just ('0', rest) -> if Text.null rest then pure 0 else Left invalid
        _ -> maybe (Left invalid) pure . Read.readMaybe $ Text.unpack text

atKey :: Token.Token -> Object.Object value -> Either String value
atKey t =
    maybe (Left $ "missing key: " <> show t) (\(Member.Member _ v) -> pure v)
        . List.find
              (\(Member.Member k _) ->
                  String.toText (Name.toString k) == Token.toText t
              )
        . Object.toList
