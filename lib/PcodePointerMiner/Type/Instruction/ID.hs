{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}

module PcodePointerMiner.Type.Instruction.ID where

import Data.Aeson   (Encoding, FromJSON, ToJSON (toEncoding), defaultOptions,
                     genericToEncoding)

import GHC.Generics (Generic)

newtype ID
  = ID Int
  deriving (Generic, Show)

instance ToJSON ID where
  toEncoding :: ID -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ID
