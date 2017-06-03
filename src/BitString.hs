module BitString where

import Data.Int

newtype BitString = BitString Word

bitStringToList :: BitString -> [Bool]
bitStringToList bs = undefined
