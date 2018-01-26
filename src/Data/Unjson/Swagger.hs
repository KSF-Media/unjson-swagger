{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Unjson.Swagger where

import           Control.Applicative.Free
import           Control.Lens
import           Data.Aeson                 (toJSON)
import qualified Data.Aeson                 as Json
import qualified Data.HashMap.Lazy          as HashMap
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import           Data.Int                   (Int16, Int32, Int64, Int8)
import           Data.Maybe                 (fromJust)
import           Data.Monoid                (All, Any)
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Scientific            (Scientific)
import           Data.Swagger               as Swagger
import           Data.Swagger.Declare       (Declare, declare, looks)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time                  (Day, LocalTime, NominalDiffTime,
                                             UTCTime, ZonedTime)
import           Data.Typeable              (TypeRep, Typeable, typeRep)
import           Data.Unjson                as Unjson
import           Data.UUID                  (UUID)
import           Data.Word                  (Word16, Word32, Word64, Word8)
import           Numeric.Natural            (Natural)

unjsonDefSchema
  :: Typeable a
  => Unjson.UnjsonDef a
  -> Declare
       (Swagger.Definitions Swagger.Schema)
       Swagger.Schema
unjsonDefSchema d = do
  referenced <- unjsonDefReferencedSchema d
  case referenced of
    Swagger.Inline s -> pure s
    Swagger.Ref (Swagger.Reference referenceName) -> do
      fromJust <$> looks (InsOrd.lookup referenceName)

unjsonDefNamedSchema
  :: Typeable a
  => Unjson.UnjsonDef a
  -> Declare
       (Swagger.Definitions Swagger.Schema)
       Swagger.NamedSchema
unjsonDefNamedSchema a =
  typeNamedSchema a <$> unjsonDefSchema a

unjsonDefReferencedNamedSchema
  :: Typeable a
  => Unjson.UnjsonDef a
  -> Declare
       (Swagger.Definitions Swagger.Schema)
       (Swagger.Referenced  Swagger.NamedSchema)
unjsonDefReferencedNamedSchema a =
  fmap (typeNamedSchema a) <$> unjsonDefReferencedSchema a

unjsonDefReferencedSchema
  :: Typeable a
  => Unjson.UnjsonDef a
  -> Declare
       (Swagger.Definitions Swagger.Schema)
       (Swagger.Referenced  Swagger.Schema)
unjsonDefReferencedSchema d =
  case d of
    Unjson.TupleUnjsonDef _tuples -> do
      pure $ Inline $ mempty
    Unjson.DisjointUnjsonDef _key alternates -> do
      pure $ Inline $ mempty
        & type_ .~ SwaggerString
        & enum_ ?~ ((\(a, _, _) -> toJSON a) <$> alternates)
    Unjson.UnionUnjsonDef _ -> do
      pure $ Inline $ mempty

    Unjson.ObjectUnjsonDef fields -> do
      let referenceName = unjsonDefName d
      objectSchema <- fieldsSchema fields
      declare [(referenceName, objectSchema)]
      pure $ Ref $ Reference referenceName

    MapUnjsonDef val _f _g -> do
      valSchema <- unjsonDefReferencedSchema val
      pure $ Inline $ mempty
        & type_ .~ SwaggerObject
        & additionalProperties ?~ valSchema

    ArrayUnjsonDef _mpk am _n _k val
      | ArrayModeStrict <- am -> do
          valSchema <- unjsonDefReferencedSchema val
          pure $ Inline $ mempty
            & type_ .~ SwaggerArray
            & items ?~ SwaggerItemsObject valSchema
      | otherwise ->
          error "Non-strict array modes aren't yet supported"

    Unjson.SimpleUnjsonDef t _f _g ->
      case t of
        "Bool"            -> declareSchemaRef $ Proxy @Bool
        "Char"            -> declareSchemaRef $ Proxy @Char
        "Double"          -> declareSchemaRef $ Proxy @Double
        "Float"           -> declareSchemaRef $ Proxy @Float
        "Int"             -> declareSchemaRef $ Proxy @Int
        "Int8"            -> declareSchemaRef $ Proxy @Int8
        "Int16"           -> declareSchemaRef $ Proxy @Int16
        "int32"           -> declareSchemaRef $ Proxy @Int32
        "Int64"           -> declareSchemaRef $ Proxy @Int64
        "Integer"         -> declareSchemaRef $ Proxy @Integer
        "Natural"         -> declareSchemaRef $ Proxy @Natural
        "Word"            -> declareSchemaRef $ Proxy @Word
        "Word8"           -> declareSchemaRef $ Proxy @Word8
        "Word16"          -> declareSchemaRef $ Proxy @Word16
        "Word32"          -> declareSchemaRef $ Proxy @Word32
        "Word64"          -> declareSchemaRef $ Proxy @Word64
        "()"              -> declareSchemaRef $ Proxy @()
        "Scientific"      -> declareSchemaRef $ Proxy @Scientific
        "String"          -> declareSchemaRef $ Proxy @String
        "[Char]"          -> declareSchemaRef $ Proxy @[Char]
        "Text"            -> declareSchemaRef $ Proxy @Text
        "UTCTime"         -> declareSchemaRef $ Proxy @UTCTime
        "Any"             -> declareSchemaRef $ Proxy @Any
        "All"             -> declareSchemaRef $ Proxy @All
        "UUID"            -> declareSchemaRef $ Proxy @UUID
        "ZonedTime"       -> declareSchemaRef $ Proxy @ZonedTime
        "LocalTime"       -> declareSchemaRef $ Proxy @LocalTime
        "NominalDiffTime" -> declareSchemaRef $ Proxy @NominalDiffTime
        "Day"             -> declareSchemaRef $ Proxy @Day
        _ ->
          -- if it's not one of the hardcoded types just place the reference to it.
          -- Unresolved references end up being shown as error in swagger-ui,
          -- which is a nicer UX than throwing an exception or something.
          pure $ Ref $ Reference t

fieldsSchema
  :: Ap (FieldDef s) a
  -> Declare (Swagger.Definitions Swagger.Schema) Swagger.Schema
fieldsSchema =
  \case
    Pure _ -> pure $ mempty & type_ .~ SwaggerObject
    Ap f r -> (<>) <$> fieldSchema f <*> fieldsSchema r

fieldSchema
  :: FieldDef s a
  -> Declare (Swagger.Definitions Swagger.Schema) Swagger.Schema
fieldSchema =
  \case
    Unjson.FieldReqDef key doc _f val -> do
      valSchema <- unjsonDefReferencedSchema val
      pure $ mempty
        & type_ .~ SwaggerObject
        & properties .~ [(key, valSchema <&> description ?~ doc)]
        & required   .~ [key]
    Unjson.FieldOptDef key doc _f val -> do
      valSchema <- unjsonDefReferencedSchema val
      pure $ mempty
        & type_ .~ SwaggerObject
        & properties .~ [(key, valSchema <&> description ?~ doc)]
    Unjson.FieldDefDef key doc _defval _f val -> do
      valSchema <- unjsonDefReferencedSchema val
      pure $ mempty
        & type_ .~ SwaggerObject
        & properties .~ [( key, valSchema <&> description ?~ doc)]
    Unjson.FieldRODef key doc _f val -> do
      valSchema <- unjsonDefReferencedSchema val
      pure $ mempty
        & type_ .~ SwaggerObject
        & properties .~ [( key, valSchema <&> readOnly ?~ True
                                          <&> description ?~ doc)]

typeNamedSchema :: forall a. Typeable a => UnjsonDef a -> Schema -> NamedSchema
typeNamedSchema = NamedSchema . Just . unjsonDefName

unjsonDefName :: forall a. Typeable a => UnjsonDef a -> Text
unjsonDefName = Text.pack . show . unjsonDefType

unjsonDefType :: forall a. Typeable a => UnjsonDef a -> TypeRep
unjsonDefType _ = typeRep (Proxy @a)
