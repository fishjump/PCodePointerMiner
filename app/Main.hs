{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson                     (eitherDecode)
import Data.ByteString.Lazy.Char8     (pack)

import PcodePointerMiner.Type.Program (Program)

main :: IO ()
main = do
  let file = "/Users/yueyu/repos/PCodePointerMiner/example/example3.json"
  jsonStr <- readFile file
  let maybeValue = eitherDecode $ pack jsonStr :: Either String Program
  case maybeValue of
    Left err    -> putStrLn err
    Right value -> print value
