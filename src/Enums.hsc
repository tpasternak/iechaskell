module Enums
where
import Prelude hiding (or,all)
import Foreign.C.Types
import Data.Word
import BitString

#include "iec61850_client.h"

newtype AcsiClass = AcsiClass { unAcsiClass :: CInt }
    deriving (Eq,Show)

#{enum AcsiClass, AcsiClass
, dataObject = ACSI_CLASS_DATA_OBJECT
, dataSet = ACSI_CLASS_DATA_SET
, brcb = ACSI_CLASS_BRCB
, urcb = ACSI_CLASS_URCB
, lcb = ACSI_CLASS_LCB
, log = ACSI_CLASS_LOG
, sgcb = ACSI_CLASS_SGCB
, gocb = ACSI_CLASS_GoCB
, gscb = ACSI_CLASS_GsCB
, msvcb = ACSI_CLASS_MSVCB
, usvcb = ACSI_CLASS_USVCB
}

newtype FunctionalConstraint = FunctionalConstraint { unFunctionalConstraint :: CInt }
  deriving (Eq)

#{enum FunctionalConstraint,FunctionalConstraint
, st = IEC61850_FC_ST
, mx = IEC61850_FC_MX
, sp = IEC61850_FC_SP
, sv = IEC61850_FC_SV
, cf = IEC61850_FC_CF
, dc = IEC61850_FC_DC
, sg = IEC61850_FC_SG
, se = IEC61850_FC_SE
, sr = IEC61850_FC_SR
, or = IEC61850_FC_OR
, bl = IEC61850_FC_BL
, ex = IEC61850_FC_EX
, co = IEC61850_FC_CO
, us = IEC61850_FC_US
, ms = IEC61850_FC_MS
, rp = IEC61850_FC_RP
, br = IEC61850_FC_BR
, lg = IEC61850_FC_LG
, all = IEC61850_FC_ALL
, none = IEC61850_FC_NONE
 }

instance Show FunctionalConstraint where
  show x
      | x == st = "ST"
      | x == mx = "MX"
      | x == sp = "SP"
      | x == sv = "SV"
      | x == cf = "CF"
      | x == dc = "DC"
      | x == sg = "SG"
      | x == se = "SE"
      | x == sr = "SR"
      | x == or = "OR"
      | x == bl = "BL"
      | x == ex = "EX"
      | x == co = "CO"
      | x == us = "US"
      | x == ms = "MS"
      | x == rp = "RP"
      | x == br = "BR"
      | x == lg = "LG"
      | x == all = "ALL"
      | x == none = "NONE"

allConstraints :: [FunctionalConstraint]
allConstraints = [st, mx, sp, sv, cf, dc, sg, se, sr, or, bl, ex, co, us, ms, rp, br, lg]


newtype MmsType = MmsType { unMmsType :: CInt }
  deriving (Eq)

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

data MmsVar =
  Array [MmsVar]
  | MmsStructure [MmsVar]
  | MmsBoolean Bool
  | MmsBitString BitString
  | MmsInteger CInt
  | Unsigned Int
  | MmsFloat Double
  | MmsOctetString 
  | MmsVisibleString String
  | MmsGeneralizedTime
  | MmsBinaryTime
  | MmsBcd
  | MmsObjId
  | MmsString String
  | MmsUtcTime Int
  | MmsUnknown String
  deriving (Show)

type CBool = #{type bool}

cFalse, cTrue :: CBool
cFalse = 0
cTrue = 1

type CUint64 = #{type uint64_t}

type CUint32 = #{type uint32_t}
