module Argo.Codec.Object where

import qualified Argo.Codec.Codec as Codec
import qualified Argo.Codec.List as Codec
import qualified Argo.Codec.Value as Codec
import qualified Argo.Json.Array as Array
import qualified Argo.Json.Boolean as Boolean
import qualified Argo.Json.Member as Member
import qualified Argo.Json.Name as Name
import qualified Argo.Json.Null as Null
import qualified Argo.Json.Object as Object
import qualified Argo.Json.String as String
import qualified Argo.Json.Value as Value
import qualified Argo.Schema.Identifier as Identifier
import qualified Argo.Schema.Schema as Schema
import qualified Argo.Type.Permission as Permission
import qualified Argo.Vendor.Text as Text
import qualified Argo.Vendor.Transformers as Trans
import qualified Control.Monad as Monad
import qualified Data.Functor.Identity as Identity
import qualified Data.List as List
import qualified Data.Maybe as Maybe

type Object a
    = Codec.List
          ( Trans.AccumT
                ()
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
                    . (,) Nothing
                    . Schema.fromValue
                    . Value.Object
                    $ Object.fromList
                          [ Member.fromTuple
                              ( Name.fromString . String.fromText $ Text.pack
                                  "type"
                              , Value.String . String.fromText $ Text.pack
                                  "object"
                              )
                          , Member.fromTuple
                              ( Name.fromString . String.fromText $ Text.pack
                                  "properties"
                              , Value.Object . Object.fromList $ fmap
                                  (\((k, _), (_, s)) ->
                                      Member.fromTuple (k, Schema.toValue s)
                                  )
                                  schemas
                              )
                          , Member.fromTuple
                              ( Name.fromString . String.fromText $ Text.pack
                                  "required"
                              , Value.Array . Array.fromList $ Maybe.mapMaybe
                                  (\((k, r), _) -> if r
                                      then Just . Value.String $ Name.toString
                                          k
                                      else Nothing
                                  )
                                  schemas
                              )
                          , Member.fromTuple
                              ( Name.fromString . String.fromText $ Text.pack
                                  "additionalProperties"
                              , Value.Boolean
                              . Boolean.fromBool
                              $ case permission of
                                    Permission.Allow -> True
                                    Permission.Forbid -> False
                              )
                          ]
            )
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
    , Codec.schema = pure . (,) (k, True) <$> Codec.schema c
    }

optional :: Name.Name -> Codec.Value a -> Object (Maybe a)
optional k c = Codec.Codec
    { Codec.decode = do
        xs <- Trans.get
        case List.partition (\(Member.Member j _) -> j == k) xs of
            (Member.Member _ x : _, ys) -> case Codec.decodeWith c x of
                Left y -> if x == Value.Null (Null.fromUnit ())
                    then pure Nothing
                    else Trans.lift $ Trans.throwE y
                Right y -> do
                    Trans.put ys
                    pure $ Just y
            _ -> pure Nothing
    , Codec.encode = \x -> do
        case x of
            Nothing -> pure ()
            Just y -> Trans.tell [Member.Member k $ Codec.encodeWith c y]
        pure x
    , Codec.schema = pure . (,) (k, False) <$> Codec.schema c
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
