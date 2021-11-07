module Argo.Encoder where

import qualified Argo.Literal as Literal
import qualified Argo.Type.Config as Config
import qualified Argo.Type.Indent as Indent
import qualified Argo.Vendor.Builder as Builder
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity
import qualified Data.Semigroup as Semigroup

type Encoder = Trans.ReaderT Config.Config (Trans.WriterT Builder.Builder Identity.Identity)

unwrap :: Config.Config -> Encoder a -> (a, Builder.Builder)
unwrap c = Identity.runIdentity . Trans.runWriterT . flip Trans.runReaderT c

run :: Config.Config -> Encoder a -> Builder.Builder
run c = snd . unwrap c

list :: Encoder () -> Encoder () -> Encoder () -> (a -> Encoder ()) -> [a] -> Encoder ()
list l r s f xs = case xs of
    [] -> do
        l
        r
    x : ys -> do
        l
        c <- Trans.ask
        let newLine = if Config.hasIndent c then Builder.word8 Literal.newLine else mempty
        Trans.local Config.increaseLevel $ do
            i <- makeIndent <$> Trans.ask
            Trans.lift . Trans.tell $ newLine <> i
            f x
            Monad.forM_ ys $ \ y -> do
                s
                Trans.lift . Trans.tell $ newLine <> i
                f y
        Trans.lift . Trans.tell $ newLine <> makeIndent c
        r

makeIndent :: Config.Config -> Builder.Builder
makeIndent x = case Config.indent x of
    Indent.Spaces y -> if y <= 0 then mempty else
        Semigroup.stimesMonoid (Config.level x)
        . Semigroup.stimes y
        $ Builder.word8 Literal.space
    Indent.Tab -> Semigroup.stimesMonoid (Config.level x)
        $ Builder.word8 Literal.horizontalTabulation
