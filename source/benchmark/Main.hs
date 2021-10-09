import Argo ()
import qualified Test.Tasty.Bench as Tasty

main :: IO ()
main = Tasty.defaultMain
    [ Tasty.bench "id" $ Tasty.whnf id ()
    ]
