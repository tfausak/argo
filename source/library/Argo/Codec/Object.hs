module Argo.Codec.Object where

import qualified Argo.Codec.Codec as Codec
import qualified Argo.Codec.List as Codec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Schema.Schema as Schema
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Text as Text

type Object a = Codec.List [Schema.Schema] (Member.Member Value.Value) a

fromObjectCodec :: Permission.Permission -> Object a -> Codec.Value a
fromObjectCodec = Codec.fromListCodec (\_ _ -> Schema.false)
    $ Codec.map Object.toList Object.fromList Codec.objectCodec

required :: Name.Name -> Codec.Value a -> Object a
required k c = Codec.Codec
    { Codec.decode = do
        m <- Codec.decode (optional k c)
        case m of
            Nothing ->
                Trans.lift
                    . Trans.throwE
                    $ "missing required member: "
                    <> show k
            Just x -> pure x
    , Codec.encode = \x -> do
        Monad.void . Codec.encode (optional k c) $ Just x
        pure x
    , Codec.schema = [Schema.comment "TODO: Argo.Codec.Object.required"]
    }

optional :: Name.Name -> Codec.Value a -> Object (Maybe a)
optional k c = Codec.Codec
    { Codec.decode = do
        xs <- Trans.get
        case List.partition (\(Member.Member j _) -> j == k) xs of
            (Member.Member _ x : _, ys) -> case Codec.decodeWith c x of
                Left y -> Trans.lift $ Trans.throwE y
                Right y -> do
                    Trans.put ys
                    pure $ Just y
            _ -> pure Nothing
    , Codec.encode = \x -> do
        case x of
            Nothing -> pure ()
            Just y -> Trans.tell [Member.Member k $ Codec.encodeWith c y]
        pure x
    , Codec.schema = [Schema.comment "TODO: Argo.Codec.Object.optional"]
    }

tagged :: String -> Codec.Value a -> Codec.Value a
tagged t c =
    Codec.map snd ((,) ())
        . fromObjectCodec Permission.Allow
        $ (,)
        <$> Codec.project
                fst
                (required
                    (Name.fromString . String.fromText $ Text.pack "type")
                    (Codec.literalCodec
                    . Value.String
                    . String.fromText
                    $ Text.pack t
                    )
                )
        <*> Codec.project
                snd
                (required
                    (Name.fromString . String.fromText $ Text.pack "value")
                    c
                )
