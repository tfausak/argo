module Argo.Internal.Codec.List where

import qualified Argo.Internal.Codec.Codec as Codec
import qualified Argo.Internal.Codec.Value as Codec
import qualified Argo.Internal.Schema.Identifier as Identifier
import qualified Argo.Internal.Schema.Schema as Schema
import qualified Argo.Internal.Type.Permission as Permission
import qualified Argo.Vendor.Map as Map
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity

type List s e a
    = Codec.Codec
          (Trans.StateT [e] (Trans.ExceptT String Identity.Identity))
          (Trans.WriterT [e] Identity.Identity)
          s
          a
          a

fromListCodec
    :: ( Permission.Permission
       -> s
       -> Trans.AccumT
              (Map.Map Identifier.Identifier Schema.Schema)
              Identity.Identity
              (Maybe Identifier.Identifier, Schema.Schema)
       )
    -> Codec.Value [e]
    -> Permission.Permission
    -> List s e a
    -> Codec.Value a
fromListCodec f ce p ca = Codec.Codec
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
    , Codec.schema = f p $ Codec.schema ca
    }
