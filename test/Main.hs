{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Lens             ((.~), (<&>), (?~), (%~))
import           Data.Function            ((&))
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.Aeson.Encode.Pretty as Json
import           Data.Proxy               (Proxy (Proxy))
import           Data.Swagger
import           Data.Swagger.Declare
import           Data.Text                (Text)
import           Data.Typeable            (Typeable, typeRep)
import           Data.Unjson
import           Data.Unjson.Swagger
import           Data.Data                (Data)
import           System.FilePath          ((<.>), (</>))

import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.Golden        as Golden

data PaperCode = HBL | VN | ON
  deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data)

instance ToSchema PaperCode where
  declareNamedSchema _ = do
    unjsonDefNamedSchema (unjsonDef @PaperCode)
      <&> schema.example ?~ unjsonToJSON unjsonDef HBL

instance Unjson PaperCode where
  unjsonDef = stringEnumUnjsonDef

data Paper = Paper
  { paperCode :: PaperCode
  , paperName :: Text
  } deriving (Show, Eq, Data)

samplePaper :: Paper
samplePaper = Paper
  { paperCode = HBL
  , paperName = "HUFVUDSTADSBLADET"
  }

instance ToSchema Paper where
  declareNamedSchema _ = do
    unjsonDefNamedSchema (unjsonDef @Paper)
      <&> schema.example ?~ unjsonToJSON unjsonDef samplePaper

instance Unjson Paper where
  unjsonDef = objectOf $ Paper
    <$> field "code" paperCode "Identifying code of the paper"
    <*> field "name" paperName "The name of the paper"

-- Package contains a Paper object
data Package = Package
  { packageName  :: Text
  , packagePaper :: Paper
  } deriving (Show, Eq, Data)

samplePackage :: Package
samplePackage = Package
  { packageName  = "HBL 365"
  , packagePaper = samplePaper
  }

instance ToSchema Package where
  declareNamedSchema _ = do
    unjsonDefNamedSchema (unjsonDef @Package)
      <&> schema.example ?~ unjsonToJSON unjsonDef samplePackage

instance Unjson Package where
  unjsonDef = objectOf $ Package
    <$> field    "name"        packageName        "Package name"
    <*> field    "paper"       packagePaper       "Paper associated with the package"

data Customer
  = RegisteredCustomer
      { registeredCustomerPackages :: [Package]
      , registeredCustomerCusno    :: Int
      , registeredCustomerEmail    :: Text
      }
  | AnonymousCustomer
      { anonymousCustomerEmail :: Text
      , anonymousCustomerAccessCode :: Text
      }
  deriving (Eq, Data)

sampleCustomer :: Customer
sampleCustomer = AnonymousCustomer "anon@ksfmedia.fi" "letmein"

instance Unjson Customer where
  unjsonDef = disjointUnionOf "type"
    [ ( "registered", unjsonIsConstrByName "RegisteredCustomer", RegisteredCustomer
           <$> field "packages" registeredCustomerPackages "Packages that the customer is subscribed to"
           <*> field "cusno"    registeredCustomerCusno    "Customer number"
           <*> field "email"    registeredCustomerEmail    "Customer email"
      )
    , ( "anonymous", unjsonIsConstrByName "AnonymousCustomer", AnonymousCustomer
           <$> field "email"      anonymousCustomerEmail      "Customer email"
           <*> field "accessCode" anonymousCustomerAccessCode "Single-use access code"
      )
    ]

instance ToSchema Customer where
  declareNamedSchema _ = do
    unjsonDefNamedSchema (unjsonDef @Customer)
      <&> schema.example ?~ unjsonToJSON unjsonDef sampleCustomer

sampleSpec :: Swagger
sampleSpec = mempty
  & info.title .~ "Sample spec"
  & addDefinition @PaperCode Proxy
  & addDefinition @Paper Proxy
  & addDefinition @Package Proxy
  & addDefinition @Customer Proxy

addDefinition :: ToSchema a => Proxy a -> Swagger -> Swagger
addDefinition a swagger =
  swagger
    & definitions %~ \defs ->
        let (moreDefs, NamedSchema maybeName schema_) = runDeclare (declareNamedSchema a) defs
        in defs <> moreDefs <> maybe mempty (\name_ -> InsOrd.singleton name_ schema_) maybeName

main :: IO ()
main = do
  Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = do
  Tasty.testGroup "unjson-swagger"
    [ Tasty.testGroup "ToSchema"
        [ testToSchema $ Proxy @PaperCode
        , testToSchema $ Proxy @Paper
        , testToSchema $ Proxy @Package
        , testToSchema $ Proxy @Customer
        ]
    , testSwagger sampleSpec
    ]

testToSchema :: (Typeable a, ToSchema a) => Proxy a -> Tasty.TestTree
testToSchema proxy =
  Golden.goldenVsString filename filepath $ do
    pure $ Json.encodePretty $ toSchema proxy
  where
    filename = show $ typeRep proxy
    filepath = "sample_schemas" </> filename <.> "json"

testSwagger :: Swagger -> Tasty.TestTree
testSwagger spec =
  Golden.goldenVsString filename filepath $ do
    pure $ Json.encodePretty spec
  where
    filename = "sample_spec.json"
    filepath = filename
