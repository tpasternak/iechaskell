import Iec61850.Client
import Iec61850.Mms
import Iec61850.Enums.FC

path = "ied1Inverter/ZINV1.ACTyp.setVal"
fc = SP
val = MmsInteger 44

main =  do
  print $ "Trying to write to '" ++ path ++ "', FC:'" ++ show fc ++ "'"
  con <- connect "localhost" 102
  writeVal con path fc val
