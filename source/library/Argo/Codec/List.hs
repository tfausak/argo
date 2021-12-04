module Argo.Codec.List where

import qualified Argo.Codec.Codec as Codec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity

type List e a
    = Codec.Codec
          (Trans.StateT [e] (Trans.ExceptT String Identity.Identity))
          (Trans.WriterT [e] Identity.Identity)
          a
          a

fromListCodec
    :: Codec.Value [e] -> Permission.Permission -> List e a -> Codec.Value a
fromListCodec ce p ca = Codec.Codec
    { Codec.decode = do
        xs <- Codec.decode ce
        case
                Identity.runIdentity . Trans.runExceptT $ Trans.runStateT
                    (Codec.decode ca)
                    xs
            of
                Left x -> Trans.lift $ Trans.throwE x
                Right (x, ys) -> do
                    case (p, ys) of
                        (Permission.Forbid, _ : _) ->
                            Trans.lift $ Trans.throwE "leftover elements"
                        _ -> pure ()
                    pure x
    , Codec.encode = \x -> do
        Monad.void
            . Codec.encode ce
            . snd
            . Identity.runIdentity
            . Trans.runWriterT
            $ Codec.encode ca x
        pure x
    }
