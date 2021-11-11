{-# OPTIONS_GHC -Wno-orphans #-}

module Argo.Orphanage () where

import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Argo
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Test.Tasty.QuickCheck as Tasty

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
