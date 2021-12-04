module Argo.Codec.Array where

import qualified Argo.Codec.Codec as Codec
import qualified Argo.Codec.List as Codec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Json.Array as Array
import qualified Argo.Json.Value as Value
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.Transformers as Trans

type Array a = Codec.List Value.Value a

fromArrayCodec :: Permission.Permission -> Array a -> Codec.Value a
fromArrayCodec = Codec.fromListCodec
    $ Codec.map Array.toList Array.fromList Codec.arrayCodec

element :: Codec.Value a -> Array a
element c = Codec.Codec
    { Codec.decode = do
        l <- Trans.get
        case l of
            [] -> Trans.lift $ Trans.throwE "unexpected empty list"
            h : t -> case Codec.decodeWith c h of
                Left y -> Trans.lift $ Trans.throwE y
                Right y -> do
                    Trans.put t
                    pure y
    , Codec.encode = \x -> do
        Trans.tell [Codec.encodeWith c x]
        pure x
    }
