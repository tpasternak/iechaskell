import Iec61850.Client
import Iec61850.Mms
import Iec61850.Enums.FC
import Data.Either.Utils(fromRight)

path = "ied1Inverter/ZINV1.ACTyp.setVal"
fc = SP
val = MmsInteger 44

main =  do
  putStrLn $ "Trying to write to '" ++ path ++ "', FC:'" ++ show fc ++ "'"
  con <- fromRight <$> connect "localhost" 102
  x <-writeVal con path fc val
  case x of
    Right _ -> putStrLn "success"
    Left err -> putStrLn $ "Error: '" ++ show err ++ "'"
