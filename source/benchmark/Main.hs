import Test.Tasty.Bench

main :: IO ()
main = defaultMain
    [ bench "id" $ whnf id ()
    ]
