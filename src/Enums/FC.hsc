module Enums.FC
where
import Prelude hiding (or,all)
import Foreign.C.Types

#include "iec61850_client.h"

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


