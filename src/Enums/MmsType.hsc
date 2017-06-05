module Enums.MmsType
where
import Prelude hiding (or,all)
import Foreign.C.Types
import Data.Word
import BitString

#include "iec61850_client.h"

newtype MmsType = MmsType { unMmsType :: CInt }
  deriving (Eq)

instance Show MmsType where
  show x
   | x == mms_array = "MMS_ARRAY"
   | x == mms_structure = "MMS_STRUCTURE"
   | x == mms_boolean = "MMS_BOOLEAN"
   | x == mms_bit_string = "MMS_BIT_STRING"
   | x == mms_integer = "MMS_INTEGER"
   | x == mms_unsigned = "MMS_UNSIGNED"
   | x == mms_float = "MMS_FLOAT"
   | x == mms_octet_string = "MMS_OCTET_STRING"
   | x == mms_visible_string = "MMS_VISIBLE_STRING"
   | x == mms_generalized_time = "MMS_GENERALIZED_TIME"
   | x == mms_binary_time = "MMS_BINARY_TIME"
   | x == mms_bcd = "MMS_BCD"
   | x == mms_obj_id = "MMS_OBJ_ID"
   | x == mms_string = "MMS_STRING"
   | x == mms_utc_time = "MMS_UTC_TIME"
   | x == mms_data_access_error = "MMS_DATA_ACCESS_ERROR"

#{enum MmsType,MmsType
,   mms_array = MMS_ARRAY
,   mms_structure = MMS_STRUCTURE
,   mms_boolean = MMS_BOOLEAN
,   mms_bit_string = MMS_BIT_STRING
,   mms_integer = MMS_INTEGER
,   mms_unsigned = MMS_UNSIGNED
,   mms_float = MMS_FLOAT
,   mms_octet_string = MMS_OCTET_STRING
,   mms_visible_string = MMS_VISIBLE_STRING
,   mms_generalized_time = MMS_GENERALIZED_TIME
,   mms_binary_time = MMS_BINARY_TIME
,   mms_bcd = MMS_BCD
,   mms_obj_id = MMS_OBJ_ID
,   mms_string = MMS_STRING
,   mms_utc_time = MMS_UTC_TIME
,   mms_data_access_error = MMS_DATA_ACCESS_ERROR
 }

