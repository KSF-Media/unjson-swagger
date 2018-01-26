{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Lens             ((.~), (<&>), (?~))
import qualified Data.Aeson               as Json
import qualified Data.Aeson.Encode.Pretty as Json
import           Data.Proxy               (Proxy (Proxy))
import           Data.Swagger
import           Data.Text                (Text)
import           Data.Typeable            (Typeable, typeRep)
import           Data.Unjson
import           Data.Unjson.Swagger
import           System.FilePath          ((<.>), (</>))

import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.Golden        as Golden

data Paper = Paper
  { paperCode :: Text
  , paperName :: Text
  } deriving (Show, Eq)

samplePaper :: Paper
samplePaper = Paper
  { paperCode = "HBL"
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
  } deriving (Show, Eq)

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

main :: IO ()
main = do
  Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = do
  Tasty.testGroup "ToSchema"
    [ testToSchema $ Proxy @Paper
    , testToSchema $ Proxy @Package
    ]

testToSchema :: (Typeable a, ToSchema a) => Proxy a -> Tasty.TestTree
testToSchema proxy =
  Golden.goldenVsString name filepath $ do
    pure $ Json.encodePretty $ toSchema proxy
  where
    name = show $ typeRep proxy
    filepath = "sample_schemas" </> name <.> "json"
