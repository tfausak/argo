{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Json.Number where

import qualified Argo.Literal as Literal
import qualified Argo.Type.Decimal as Decimal
import qualified Argo.Type.Decoder as Decoder
import qualified Argo.Type.Encoder as Encoder
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.ByteString as ByteString
import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Bool as Bool
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import qualified GHC.Generics as Generics

newtype Number
    = Number Decimal.Decimal
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

fromDecimal :: Decimal.Decimal -> Number
fromDecimal = Number

toDecimal :: Number -> Decimal.Decimal
toDecimal (Number x) = x

encode :: Number -> Encoder.Encoder ()
encode x = do
    let Decimal.Decimal s e = toDecimal x
    Trans.lift . Trans.tell $ Builder.integerDec s
    Monad.when (e /= 0)
        . Trans.lift
        . Trans.tell
        $ Builder.word8 Literal.latinSmallLetterE
        <> Builder.integerDec e

decode :: Decoder.Decoder Number
decode = do
    ni <- fmap Maybe.isJust . Applicative.optional $ Decoder.word8
        Literal.hyphenMinus
    i <- Decoder.takeWhile1 Decoder.isDigit
    Monad.when
            (ByteString.length i
            > 1
            && ByteString.elemIndex Literal.digitZero i
            == Just 0
            )
        . Trans.lift
        $ Trans.throwE "leading zero"
    f <- fmap (Maybe.fromMaybe ByteString.empty) . Applicative.optional $ do
        Decoder.word8 Literal.fullStop
        Decoder.takeWhile1 Decoder.isDigit
    (ne, e) <-
        fmap (Maybe.fromMaybe (False, ByteString.empty))
        . Applicative.optional
        $ do
              Monad.void
                  . Decoder.satisfy
                  $ \x ->
                        (x == Literal.latinSmallLetterE)
                            || (x == Literal.latinCapitalLetterE)
              ne <-
                  fmap (== Just Literal.hyphenMinus)
                  . Applicative.optional
                  . Decoder.satisfy
                  $ \x -> x == Literal.hyphenMinus || x == Literal.plusSign
              e <- Decoder.takeWhile1 Decoder.isDigit
              pure (ne, e)
    Decoder.spaces
    pure . fromDecimal $ Decimal.decimal
        (negateIf ni $ (fromDigits i * 10 ^ ByteString.length f) + fromDigits f
        )
        (negateIf ne (fromDigits e) - intToInteger (ByteString.length f))

negateIf :: Bool -> Integer -> Integer
negateIf = Bool.bool id negate

fromDigits :: ByteString.ByteString -> Integer
fromDigits =
    ByteString.foldl' (\a e -> (a * 10) + word8ToInteger (e - 0x30)) 0

intToInteger :: Int -> Integer
intToInteger = fromIntegral

word8ToInteger :: Word.Word8 -> Integer
word8ToInteger = fromIntegral
