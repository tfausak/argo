module Argo.Type.Number where

import qualified Argo.Decoder as Decoder
import qualified Argo.Literal as Literal
import qualified Control.Applicative as Applicative
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Bool as Bool
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word

data Number
    = Number Integer Integer
    deriving (Eq, Show)

instance DeepSeq.NFData Number where
    rnf (Number x y) = DeepSeq.deepseq x $ DeepSeq.rnf y

normalize :: Number -> Number
normalize (Number x y) =
    if x == 0
    then Number 0 0
    else let (q, r) = quotRem x 10 in if r == 0
    then normalize $ Number q (y + 1)
    else Number x y

encode :: Number -> Builder.Builder
encode (Number x y) =
    if y == 0
    then Builder.integerDec x
    else Builder.integerDec x
        <> Builder.word8 Literal.latinSmallLetterE
        <> Builder.integerDec y

decode :: Decoder.Decoder Number
decode = do
    ni <- fmap Maybe.isJust . Applicative.optional $ Decoder.word8 Literal.hyphenMinus
    i <- Decoder.takeWhile1 Decoder.isDigit
    Monad.when (ByteString.length i > 1 && ByteString.elemIndex Literal.digitZero i == Just 0)
        $ fail "leading zero"
    f <- fmap (Maybe.fromMaybe ByteString.empty) . Applicative.optional $ do
        Decoder.word8 Literal.fullStop
        Decoder.takeWhile1 Decoder.isDigit
    (ne, e) <- fmap (Maybe.fromMaybe (False, ByteString.empty)) . Applicative.optional $ do
        Monad.void
            . Decoder.satisfy
            $ \ x -> x == Literal.latinSmallLetterE || x == Literal.latinCapitalLetterE
        ne <- fmap (== Just Literal.hyphenMinus)
            . Applicative.optional
            . Decoder.satisfy
            $ \ x -> x == Literal.hyphenMinus || x == Literal.plusSign
        e <- Decoder.takeWhile1 Decoder.isDigit
        pure (ne, e)
    Decoder.spaces
    pure . normalize $ Number
        (negateIf ni $ (fromDigits i * 10 ^ ByteString.length f) + fromDigits f)
        (negateIf ne (fromDigits e) - intToInteger (ByteString.length f))

negateIf :: Bool -> Integer -> Integer
negateIf = Bool.bool id negate

fromDigits :: ByteString.ByteString -> Integer
fromDigits = ByteString.foldl' (\ a e -> (a * 10) + word8ToInteger (e - 0x30)) 0

intToInteger :: Int -> Integer
intToInteger = fromIntegral

word8ToInteger :: Word.Word8 -> Integer
word8ToInteger = fromIntegral
