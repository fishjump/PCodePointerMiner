{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module PcodePointerMiner.Type.Function where

import Data.Aeson                         (Encoding, FromJSON (parseJSON),
                                           ToJSON (toEncoding), defaultOptions,
                                           genericToEncoding, withObject, (.:))
import Data.Text                          (Text)

import GHC.Generics                       (Generic)

import PcodePointerMiner.Type.BasicBlock  (BasicBlock)
import PcodePointerMiner.Type.Instruction (Instruction)
import PcodePointerMiner.Type.Varnode     (Varnode)

data Function = Function
  { __type__     :: Text
  , name         :: Text
  , varnodes     :: [Varnode]
  , instructions :: [Instruction]
  , basicBlocks  :: [BasicBlock]
  , raw          :: [Text]
  }
  deriving (Generic, Show)

instance ToJSON Function where
  toEncoding :: Function -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Function where
  parseJSON = withObject "Function" $ \v -> do
    __type__ <- v .: "type"
    name <- v .: "name"
    varnodes <- v .: "variables"
    instructions <- v .: "instructions"
    basicBlocks <- v .: "basic-blocks"
    raw <- v .: "raw"
    return Function {__type__ = __type__, name = name, varnodes = varnodes, instructions = instructions, basicBlocks = basicBlocks, raw = raw}
