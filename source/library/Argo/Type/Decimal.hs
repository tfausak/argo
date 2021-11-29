{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}

module Argo.Type.Decimal where

import Data.Ratio ((%))

import qualified Argo.Vendor.DeepSeq as DeepSeq
import qualified Argo.Vendor.TemplateHaskell as TH
import qualified Data.List as List
import qualified Data.Ratio as Ratio
import qualified GHC.Generics as Generics
import qualified Numeric

data Decimal = Decimal Integer Integer
    deriving (Eq, Generics.Generic, TH.Lift, DeepSeq.NFData, Show)

negate :: Decimal -> Decimal
negate (Decimal s e) = Decimal (-s) e

decimal :: Integer -> Integer -> Decimal
decimal s = normalize . Decimal s

normalize :: Decimal -> Decimal
normalize (Decimal s e) = if s == 0
    then Decimal 0 0
    else
        let (q, r) = quotRem s 10
        in if r == 0 then normalize $ Decimal q (e + 1) else Decimal s e

fromInteger :: Integer -> Decimal
fromInteger = flip decimal 0

toInteger :: Decimal -> Maybe Integer
toInteger (Decimal s e) = if e < 0 then Nothing else Just $ s * 10 ^ e

fromRealFloat :: RealFloat a => a -> Maybe Decimal
fromRealFloat x = if isNaN x || isInfinite x
    then Nothing
    else
        Just
        . (if x < 0 then Argo.Type.Decimal.negate else id)
        . uncurry fromDigits
        . Numeric.floatToDigits 10
        $ abs x

toRealFloat :: RealFloat a => Decimal -> a
toRealFloat = Prelude.fromRational . Argo.Type.Decimal.toRational

fromDigits :: [Int] -> Int -> Decimal
fromDigits ds e = uncurry decimal $ List.foldl'
    (\(a, n) d -> (a * 10 + Prelude.toInteger d, n - 1))
    (0, Prelude.toInteger e)
    ds

toRational :: Decimal -> Rational
toRational d@(Decimal s e) =
    maybe (s % (10 ^ (-e))) fromIntegral $ Argo.Type.Decimal.toInteger d

fromRational :: Rational -> Maybe Decimal
fromRational r =
    let
        n = Ratio.numerator r
        d1 = Ratio.denominator r
        (t, d2) = factor 2 (0 :: Integer) d1
        (f, d3) = factor 5 (0 :: Integer) d2
        p = max t f
    in if d3 == 1
        then Just $ decimal (n * 2 ^ (p - t) * 5 ^ (p - f)) (-p)
        else Nothing

-- factor d 0 x = (p, y) <=> x = (d ^ p) * y
factor :: (Num a, Integral b) => b -> a -> b -> (a, b)
factor d n x =
    let (q, r) = quotRem x d
    in if x /= 0 && r == 0 then factor d (n + 1) q else (n, x)
