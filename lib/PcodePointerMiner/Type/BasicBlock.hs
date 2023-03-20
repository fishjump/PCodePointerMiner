{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module PcodePointerMiner.Type.BasicBlock where

import           Data.Aeson                            (Encoding,
                                                        FromJSON (parseJSON),
                                                        ToJSON (toEncoding),
                                                        defaultOptions,
                                                        genericToEncoding,
                                                        withObject, (.:))
import           Data.Text.Lazy                        (Text)

import           GHC.Generics                          (Generic)

import           PcodePointerMiner.Type.BasicBlock.ID  (ID)
import qualified PcodePointerMiner.Type.Instruction.ID as Instruction

import           Prelude                               hiding (id)

data BasicBlock = BasicBlock
  { __type__     :: Text
  , id           :: ID
  , preds        :: [ID]
  , succs        :: [ID]
  , instructions :: [Instruction.ID]
  }
  deriving (Generic, Show)

instance ToJSON BasicBlock where
  toEncoding :: BasicBlock -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON BasicBlock where
  parseJSON = withObject "BasicBlock" $ \v -> do
    __type__ <- v .: "type"
    id <- v .: "id"
    preds <- v .: "preds"
    succs <- v .: "succs"
    instructions <- v .: "instructions"
    return BasicBlock {__type__ = __type__, id = id, preds = preds, succs = succs, instructions = instructions}
