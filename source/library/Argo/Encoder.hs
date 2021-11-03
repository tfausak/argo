module Argo.Encoder where

import qualified Argo.Literal as Literal
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity
import qualified Data.Semigroup as Semigroup

type Encoder = Trans.ReaderT Config (Trans.WriterT Builder.Builder Identity.Identity)

data Config = Config
    { indent :: Indent
    , level :: Int
    } deriving (Eq, Show)

data Indent
    = Spaces Int
    | Tab
    deriving (Eq, Show)

hasIndent :: Config -> Bool
hasIndent x = case indent x of
    Spaces y -> y > 0
    Tab -> True

increaseLevel :: Config -> Config
increaseLevel x = x { level = level x + 1 }

list :: Encoder () -> Encoder () -> Encoder () -> (a -> Encoder ()) -> [a] -> Encoder ()
list l r s f xs = case xs of
    [] -> do
        l
        r
    x : ys -> do
        l
        c <- Trans.ask
        let newLine = if hasIndent c then Builder.word8 Literal.newLine else mempty
        Trans.local increaseLevel $ do
            i <- makeIndent <$> Trans.ask
            Trans.lift . Trans.tell $ newLine <> i
            f x
            Monad.forM_ ys $ \ y -> do
                s
                Trans.lift . Trans.tell $ newLine <> i
                f y
        Trans.lift . Trans.tell $ newLine <> makeIndent c
        r

makeIndent :: Config -> Builder.Builder
makeIndent x = case indent x of
    Spaces y -> if y <= 0 then mempty else
        Semigroup.stimesMonoid (level x)
        . Semigroup.stimes y
        $ Builder.word8 Literal.space
    Tab -> Semigroup.stimesMonoid (level x)
        $ Builder.word8 Literal.horizontalTabulation
