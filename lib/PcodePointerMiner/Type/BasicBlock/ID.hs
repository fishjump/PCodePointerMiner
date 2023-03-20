{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}

module PcodePointerMiner.Type.BasicBlock.ID where

import Data.Aeson     (Encoding, FromJSON, ToJSON (toEncoding), defaultOptions,
                       genericToEncoding)
import Data.Text.Lazy (Text)

import GHC.Generics   (Generic)

-- BasicBlock ID
newtype ID
  = ID Text
  deriving (Generic, Show)

instance ToJSON ID where
  toEncoding :: ID -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ID
