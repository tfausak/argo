import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Argo"
    [ testCase "succeeds" $ do
        1 + 2 @?= 3
    ]
