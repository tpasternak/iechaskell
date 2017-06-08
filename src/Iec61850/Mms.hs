module Iec61850.Mms where

import           Data.Int
import           Iec61850.BitString

data MmsVar = Array [MmsVar]
            | MmsStructure [MmsVar]
            | MmsBoolean Bool
            | MmsBitString BitString
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
  deriving (Show)
