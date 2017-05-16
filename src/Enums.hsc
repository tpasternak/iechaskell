module Enums
where
import Prelude hiding (or,all)
import Foreign.C.Types

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
, cd = IEC61850_FC_DC
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
      | x == cd = "DC"
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
