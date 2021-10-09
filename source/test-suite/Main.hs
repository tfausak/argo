import Argo ()
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Argo"
    [ Tasty.testCase "succeeds" $ do
        1 + 2 ?= (3 :: Int)
    ]

(?=) :: (Eq a, Show a) => a -> a -> Tasty.Assertion
(?=) = (Tasty.@?=)
infix 1 ?=
