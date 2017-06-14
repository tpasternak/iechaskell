module Iec61850.BitString where

import           Data.Bits
import           Data.Int

newtype BitString = BitString Word

bitStringToList :: BitString -> [Bool]
bitStringToList (BitString bs) = map (\x -> 0 /= bs .&. shift 1 x) [0 .. 31]

instance Show BitString where
  show = map
              (\x -> if x
                       then '1'
                       else '0') . bitStringToList
