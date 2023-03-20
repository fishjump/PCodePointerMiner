{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module PcodePointerMiner.Type.Instruction (Instruction (Instruction)) where

import           Control.Applicative                          ((<|>))

import           Data.Aeson                                   (Encoding,
                                                               FromJSON (parseJSON),
                                                               ToJSON (toEncoding),
                                                               Value,
                                                               defaultOptions,
                                                               genericToEncoding,
                                                               withObject, (.:),
                                                               (.:?))
import           Data.Aeson.Types                             (Parser)
import           Data.Text.Lazy                               (Text)

import           GHC.Generics                                 (Generic)

import qualified PcodePointerMiner.Type.BasicBlock.ID         as BasicBlock
import           PcodePointerMiner.Type.Instruction.ID        (ID)
import           PcodePointerMiner.Type.Instruction.Operation (Operation)
import           PcodePointerMiner.Type.Varnode               (Varnode)

import           Prelude                                      hiding (id)

data Instruction = Instruction
  { __type__  :: Text
  , id        :: ID
  , parent    :: BasicBlock.ID
  , operation :: Operation
  , reuslt    :: Maybe Varnode
  , operands  :: [Varnode]
  , preds     :: [ID]
  , succs     :: [ID]
  }
  deriving (Generic, Show)

instance ToJSON Instruction where
  toEncoding :: Instruction -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Instruction where
  parseJSON :: Value -> Parser Instruction
  parseJSON = withObject "Instruction" $ \v -> do
    __type__ <- v .: "type"
    id <- v .: "id"
    parent <- v .: "parent"
    operation <- v .: "operation"
    reuslt <- v .:? "reuslt" <|> pure Nothing
    operands <- v .: "operands"
    preds <- v .: "preds"
    succs <- v .: "succs"
    return Instruction {__type__ = __type__, id = id, parent = parent, operation = operation, reuslt = reuslt, operands = operands, preds = preds, succs = succs}
