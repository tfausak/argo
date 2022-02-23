module Argo.Codec.Object where

import qualified Argo.Codec.Codec as Codec
import qualified Argo.Codec.List as Codec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Schema.Identifier as Identifier
import qualified Argo.Schema.Schema as Schema
import qualified Argo.Type.Optional as Optional
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.Map as Map
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Object a
    = Codec.List
          ( Trans.AccumT
                (Map.Map Identifier.Identifier Schema.Schema)
                Identity.Identity
                [ ( (Name.Name, Bool)
                  , (Maybe Identifier.Identifier, Schema.Schema)
                  )
                ]
          )
          (Member.Member Value.Value)
          a

fromObjectCodec :: Permission.Permission -> Object a -> Codec.Value a
fromObjectCodec =
    Codec.fromListCodec
            (\permission schemasM -> do
                schemas <- schemasM
                pure
                    . Schema.unidentified
                    . Schema.Object
                          (fmap
                              (\((k, _), s) -> (k, Schema.maybeRef s))
                              schemas
                          )
                          (Maybe.mapMaybe
                              (\((k, r), _) -> if r then Just k else Nothing)
                              schemas
                          )
                    $ case permission of
                          Permission.Allow -> Nothing
                          Permission.Forbid -> Just Schema.False
            )
        $ Codec.map Object.toList Object.fromList Codec.objectCodec

required :: Name.Name -> Codec.Value a -> Object a
required k c = Codec.Codec
    { Codec.decode = do
        m <- Codec.decode (optional k c)
        case Optional.toMaybe m of
            Nothing ->
                Trans.lift
                    . Trans.throwE
                    $ "missing required member: "
                    <> show k
            Just x -> pure x
    , Codec.encode = \x -> do
        Monad.void . Codec.encode (optional k c) $ Optional.just x
        pure x
    , Codec.schema =
        pure . (,) (k, True) . Schema.unidentified <$> Codec.getRef c
    }

optional :: Name.Name -> Codec.Value a -> Object (Optional.Optional a)
optional k c = Codec.Codec
    { Codec.decode = do
        xs <- Trans.get
        case List.partition (\(Member.Member j _) -> j == k) xs of
            (Member.Member _ x : _, ys) -> case Codec.decodeWith c x of
                Left y -> Trans.lift $ Trans.throwE y
                Right y -> do
                    Trans.put ys
                    pure $ Optional.just y
            _ -> pure Optional.nothing
    , Codec.encode = \x -> do
        case Optional.toMaybe x of
            Nothing -> pure ()
            Just y -> Trans.tell [Member.Member k $ Codec.encodeWith c y]
        pure x
    , Codec.schema =
        pure . (,) (k, False) . Schema.unidentified <$> Codec.getRef c
    }

tagged :: String -> Codec.Value a -> Codec.Value a
tagged t c =
    Codec.map snd ((,) ())
        . fromObjectCodec Permission.Forbid
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
