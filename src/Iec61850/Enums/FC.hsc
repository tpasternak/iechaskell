module Iec61850.Enums.FC
where
import Prelude hiding (or,all)
import Foreign.C.Types

#include "iec61850_client.h"

newtype FunctionalConstraint_ = FunctionalConstraint_ { unFunctionalConstraint :: CInt }
  deriving (Eq)

data FunctionalConstraint= ST
  | MX
  | SP
  | SV
  | CF
  | DC
  | SG
  | SE
  | SR
  | OR
  | BL
  | EX
  | CO
  | US
  | MS
  | RP
  | BR
  | LG
  | ALL
  | NONE deriving(Show,Eq,Enum,Read)

#{enum FunctionalConstraint_,FunctionalConstraint_
, st_ = IEC61850_FC_ST
, mx_ = IEC61850_FC_MX
, sp_ = IEC61850_FC_SP
, sv_ = IEC61850_FC_SV
, cf_ = IEC61850_FC_CF
, dc_ = IEC61850_FC_DC
, sg_ = IEC61850_FC_SG
, se_ = IEC61850_FC_SE
, sr_ = IEC61850_FC_SR
, or_ = IEC61850_FC_OR
, bl_ = IEC61850_FC_BL
, ex_ = IEC61850_FC_EX
, co_ = IEC61850_FC_CO
, us_ = IEC61850_FC_US
, ms_ = IEC61850_FC_MS
, rp_ = IEC61850_FC_RP
, br_ = IEC61850_FC_BR
, lg_ = IEC61850_FC_LG
, all_ = IEC61850_FC_ALL
, none_ = IEC61850_FC_NONE
 }

fromInt x
      | x == st_ = ST
      | x == mx_ = MX
      | x == sp_ = SP
      | x == sv_ = SV
      | x == cf_ = CF
      | x == dc_ = DC
      | x == sg_ = SG
      | x == se_ = SE
      | x == sr_ = SR
      | x == or_ = OR
      | x == bl_ = BL
      | x == ex_ = EX
      | x == co_ = CO
      | x == us_ = US
      | x == ms_ = MS
      | x == rp_ = RP
      | x == br_ = BR
      | x == lg_ = LG

toInt = unFunctionalConstraint . toInt'

toInt' x
      | x == ST = st_  
      | x == MX = mx_  
      | x == SP = sp_  
      | x == SV = sv_  
      | x == CF = cf_  
      | x == DC = dc_  
      | x == SG = sg_  
      | x == SE = se_  
      | x == SR = sr_  
      | x == OR = or_  
      | x == BL = bl_  
      | x == EX = ex_  
      | x == CO = co_  
      | x == US = us_  
      | x == MS = ms_  
      | x == RP = rp_  
      | x == BR = br_  
      | x == LG = lg_       

readFC x
      | x ==  "ST" = ST 
      | x ==  "MX" = MX 
      | x ==  "SP" = SP 
      | x ==  "SV" = SV 
      | x ==  "CF" = CF 
      | x ==  "DC" = DC 
      | x ==  "SG" = SG 
      | x ==  "SE" = SE 
      | x ==  "SR" = SR 
      | x ==  "OR" = OR 
      | x ==  "BL" = BL 
      | x ==  "EX" = EX 
      | x ==  "CO" = CO 
      | x ==  "US" = US 
      | x ==  "MS" = MS 
      | x ==  "RP" = RP 
      | x ==  "BR" = BR 
      | x ==  "LG" = LG 
      | x ==  "ALL" = ALL
      | x == "NONE" =NONE 

allConstraints :: [FunctionalConstraint]
allConstraints = [ST, MX, SP, SV, CF, DC, SG, SE, SR, OR, BL, EX, CO, US, MS, RP, BR, LG]


