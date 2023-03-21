{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}

module PcodePointerMiner.Type.Instruction.Operation (Operation) where

import Data.Aeson       (Encoding, FromJSON (parseJSON), ToJSON (toEncoding),
                         Value, defaultOptions, genericToEncoding, withText)
import Data.Aeson.Types (Parser)

import GHC.Generics     (Generic)

data Operation
  -- Integer comparisons
  = INT_EQUAL
  | INT_NOTEQUAL
  | INT_LESS
  | INT_SLESS
  | INT_LESSEQUAL
  | INT_SLESSEQUAL
  | INT_CARRY
  | INT_SCARRY
  | INT_SBORROW
  -- Integer binary operations
  | INT_ADD
  | INT_SUB
  | INT_XOR
  | INT_AND
  | INT_OR
  | INT_LEFT
  | INT_RIGHT
  | INT_SRIGHT
  | INT_MULT
  | INT_DIV
  | INT_REM
  | INT_SDIV
  | INT_SREM
  -- Integer unary operations
  | INT_ZEXT
  | INT_SEXT
  | INT_2COMP
  | INT_NEGATE
  -- Floating-point comparisons
  | FLOAT_EQUAL
  | FLOAT_NOT_EQUAL
  | FLOAT_LESS
  | FLOAT_LESS_EQUAL
  -- Floating-point binary operations
  | FLOAT_ADD
  | FLOAT_SUB
  | FLOAT_MULT
  | FLOAT_DIV
  -- Floating-point unary operations
  | FLOAT_NEG
  | FLOAT_ABS
  | FLOAT_SQRT
  | FLOAT_CEIL
  | FLOAT_FLOOR
  | FLOAT_ROUND
  | FLOAT_NAN
  -- Boolean operations
  | BOOL_NEGATE
  | BOOL_XOR
  | BOOL_AND
  | BOOL_OR
  -- Branch operations
  | BRANCH
  | CBRANCH
  | BRANCHIND
  | CALL
  | CALLIND
  | RETURN
  -- Cast operations
  | INT2FLOAT
  | FLOAT2FLOAT
  | TRUNC
  -- Piece operations (what is this?)
  | PIECE
  | SUBPIECE
  -- Pointer related operations
  | LOAD
  | STORE
  | PTRADD
  | PTRSUB
  -- Other operations
  | COPY
  -- Unknown operation
  | UNKNOWN
  deriving (Generic, Show)

instance ToJSON Operation where
  toEncoding :: Operation -> Encoding
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Operation where
  parseJSON :: Value -> Parser Operation
  parseJSON = withText "Operation" $ \t -> do
    return $ case t of
      -- Integer comparisons
      "INT_EQUAL"        ->  INT_EQUAL
      "INT_NOTEQUAL"     ->  INT_NOTEQUAL
      "INT_LESS"         ->  INT_LESS
      "INT_SLESS"        ->  INT_SLESS
      "INT_LESSEQUAL"    ->  INT_LESSEQUAL
      "INT_SLESSEQUAL"   ->  INT_SLESSEQUAL
      "INT_CARRY"        ->  INT_CARRY
      "INT_SCARRY"       ->  INT_SCARRY
      "INT_SBORROW"      ->  INT_SBORROW
      -- Integer binary operations
      "INT_ADD"          ->  INT_ADD
      "INT_SUB"          ->  INT_SUB
      "INT_XOR"          ->  INT_XOR
      "INT_AND"          ->  INT_AND
      "INT_OR"           ->  INT_OR
      "INT_LEFT"         ->  INT_LEFT
      "INT_RIGHT"        ->  INT_RIGHT
      "INT_SRIGHT"       ->  INT_SRIGHT
      "INT_MULT"         ->  INT_MULT
      "INT_DIV"          ->  INT_DIV
      "INT_REM"          ->  INT_REM
      "INT_SDIV"         ->  INT_SDIV
      "INT_SREM"         ->  INT_SREM
      -- Integer unary operations
      "INT_ZEXT"         ->  INT_ZEXT
      "INT_SEXT"         ->  INT_SEXT
      "INT_2COMP"        ->  INT_2COMP
      "INT_NEGATE"       ->  INT_NEGATE
      -- Floating-point comparisons
      "FLOAT_EQUAL"      ->  FLOAT_EQUAL
      "FLOAT_NOT_EQUAL"  ->  FLOAT_NOT_EQUAL
      "FLOAT_LESS"       ->  FLOAT_LESS
      "FLOAT_LESS_EQUAL" ->  FLOAT_LESS_EQUAL
      -- Floating-point binary operations
      "FLOAT_ADD"        ->  FLOAT_ADD
      "FLOAT_SUB"        ->  FLOAT_SUB
      "FLOAT_MULT"       ->  FLOAT_MULT
      "FLOAT_DIV"        ->  FLOAT_DIV
      -- Floating-point unary operations
      "FLOAT_NEG"        ->  FLOAT_NEG
      "FLOAT_ABS"        ->  FLOAT_ABS
      "FLOAT_SQRT"       ->  FLOAT_SQRT
      "FLOAT_CEIL"       ->  FLOAT_CEIL
      "FLOAT_FLOOR"      ->  FLOAT_FLOOR
      "FLOAT_ROUND"      ->  FLOAT_ROUND
      "FLOAT_NAN"        ->  FLOAT_NAN
      -- Boolean operations
      "BOOL_NEGATE"      ->  BOOL_NEGATE
      "BOOL_XOR"         ->  BOOL_XOR
      "BOOL_AND"         ->  BOOL_AND
      "BOOL_OR"          ->  BOOL_OR
      -- Branch operations
      "BRANCH"           ->  BRANCH
      "CBRANCH"          ->  CBRANCH
      "BRANCHIND"        ->  BRANCHIND
      "CALL"             ->  CALL
      "CALLIND"          ->  CALLIND
      "RETURN"           ->  RETURN
      -- Cast operations
      "INT2FLOAT"        -> INT2FLOAT
      "FLOAT2FLOAT"      -> FLOAT2FLOAT
      "TRUNC"            -> TRUNC
      -- Piece operations (what is this?)
      "PIECE"            -> PIECE
      "SUBPIECE"         -> SUBPIECE
      -- Pointer related operations
      "LOAD"             -> LOAD
      "STORE"            -> STORE
      "PTRADD"           -> PTRADD
      "PTRSUB"           -> PTRSUB
      -- Other operations
      "COPY"             -> COPY
      -- Unknown operation
      _                  -> UNKNOWN

