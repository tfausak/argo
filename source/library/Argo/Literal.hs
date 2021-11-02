module Argo.Literal where

import qualified Argo.Vendor.ByteString as ByteString
import qualified Data.Word as Word

backspace :: Word.Word8
backspace = 0x08

carriageReturn :: Word.Word8
carriageReturn = 0x0d

colon :: Word.Word8
colon = 0x3a

comma :: Word.Word8
comma = 0x2c

digitNine :: Word.Word8
digitNine = 0x39

digitZero :: Word.Word8
digitZero = 0x30

false :: ByteString.ByteString
false = ByteString.pack [0x66, 0x61, 0x6c, 0x73, 0x65]

formFeed :: Word.Word8
formFeed = 0x0c

fullStop :: Word.Word8
fullStop = 0x2e

horizontalTabulation :: Word.Word8
horizontalTabulation = 0x09

hyphenMinus :: Word.Word8
hyphenMinus = 0x2d

latinCapitalLetterE :: Word.Word8
latinCapitalLetterE = 0x45

latinSmallLetterB :: Word.Word8
latinSmallLetterB = 0x62

latinSmallLetterE :: Word.Word8
latinSmallLetterE = 0x65

latinSmallLetterF :: Word.Word8
latinSmallLetterF = 0x66

latinSmallLetterN :: Word.Word8
latinSmallLetterN = 0x6e

latinSmallLetterR :: Word.Word8
latinSmallLetterR = 0x72

latinSmallLetterT :: Word.Word8
latinSmallLetterT = 0x74

latinSmallLetterU :: Word.Word8
latinSmallLetterU = 0x75

leftCurlyBracket :: Word.Word8
leftCurlyBracket = 0x7b

leftSquareBracket :: Word.Word8
leftSquareBracket = 0x5b

newLine :: Word.Word8
newLine = 0x0a

null :: ByteString.ByteString
null = ByteString.pack [0x6e, 0x75, 0x6c, 0x6c]

plusSign :: Word.Word8
plusSign = 0x2b

quotationMark :: Word.Word8
quotationMark = 0x22

reverseSolidus :: Word.Word8
reverseSolidus = 0x5c

rightCurlyBracket :: Word.Word8
rightCurlyBracket = 0x7d

rightSquareBracket :: Word.Word8
rightSquareBracket = 0x5d

space :: Word.Word8
space = 0x20

true :: ByteString.ByteString
true = ByteString.pack [0x74, 0x72, 0x75, 0x65]
