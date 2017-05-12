module Enums
where

import Foreign.C.Types



#include "iec61850_client.h"

newtype AcsiClass = AcsiClass { unAcsiClass :: CInt }
    deriving (Eq,Show)

-- file: ch17/Regex-hsc.hs
-- PCRE compile options
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
