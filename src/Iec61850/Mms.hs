{-# LANGUAGE DeriveDataTypeable #-}

module Iec61850.Mms where

import           Data.Int
import           Data.Word
import           Data.Data
import           Data.Typeable
import           Iec61850.BitString

data MmsVar = Array [MmsVar]
            | MmsStructure [MmsVar]
            | MmsBoolean Bool
            | MmsBitString Word32
            | MmsInteger Int
            | MmsUnsigned Word
            | MmsFloat Double
            | MmsOctetString
            | MmsVisibleString String
            | MmsGeneralizedTime
            | MmsBinaryTime
            | MmsBcd
            | MmsObjId
            | MmsString String
            | MmsUtcTime Int64
            | MmsUnknown String
  deriving (Show, Read, Data, Typeable)
