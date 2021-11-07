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
list left right separator encode xs = do
    left
    case xs of
        [] -> pure ()
        x : ys -> do
            Trans.local Config.increaseLevel $ do
                maybeIndent <- makeIndent <$> Trans.ask
                withNewLine maybeIndent
                encode x
                Monad.forM_ ys $ \ y -> do
                    separator
                    withNewLine maybeIndent
                    encode y
            maybeIndent <- makeIndent <$> Trans.ask
            withNewLine maybeIndent
    right

makeIndent :: Config.Config -> Maybe Builder.Builder
makeIndent config = case Config.indent config of
    Indent.Spaces spaces -> if spaces <= 0
        then Nothing
        else Just
            . Semigroup.stimesMonoid (Config.level config * spaces)
            $ Builder.word8 Literal.space
    Indent.Tab -> Just
        . Semigroup.stimesMonoid (Config.level config)
        $ Builder.word8 Literal.horizontalTabulation

withNewLine :: Maybe Builder.Builder -> Encoder ()
withNewLine = maybe (pure ())
    $ Trans.lift
    . Trans.tell
    . mappend (Builder.word8 Literal.newLine)
