{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module PcodePointerMiner.Type.Program where

import Data.Aeson                      (Encoding, FromJSON (parseJSON),
                                        ToJSON (toEncoding), Value,
                                        defaultOptions, genericToEncoding,
                                        withObject, (.:))
import Data.Aeson.Types                (Parser)
import Data.Text                       (Text)

import GHC.Generics                    (Generic)

import PcodePointerMiner.Type.Function (Function)

data Program = Program
  { __type__  :: Text
  , functions :: [Function]
  }
  deriving (Generic, Show)

instance ToJSON Program where
  toEncoding :: Program -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Program where
  parseJSON :: Value -> Parser Program
  parseJSON = withObject "Program" $ \v -> do
    __type__ <- v .: "type"
    functions <- v .: "functions"
    return Program {__type__ = __type__, functions = functions}
