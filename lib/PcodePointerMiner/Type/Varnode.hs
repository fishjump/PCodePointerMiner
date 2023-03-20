{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs  #-}

{-

Module: PcodePointerMiner.Type.Variable

A pcode varnode looks like (const, 0x0, 1), in the format of (space, address,
size). Currently, we only care about them as a whole, so we just use a string
to represent them. We may need to change this in the future.

-}

module PcodePointerMiner.Type.Varnode where

import Data.Aeson     (Encoding, FromJSON, ToJSON (toEncoding), defaultOptions,
                       genericToEncoding)
import Data.Text.Lazy (Text)

import GHC.Generics   (Generic)

newtype Varnode
  = Varnode Text
  deriving (Generic, Show)

--  data Varnode = Varnode deriving (Show, Generic)

instance ToJSON Varnode where
  toEncoding :: Varnode -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Varnode
