import Iec61850.Client
import Iec61850.Mms
import Iec61850.Enums.FC
import Control.Monad.Except(runExceptT)
import System.IO(stderr,hPutStr)

path = "ied1Inverter/ZINV1.ACTyp.setVal"
fc = SP
val = MmsInteger 44

main = do
  putStrLn $ "Trying to write to '" ++ path ++ "', FC:'" ++ show fc ++ "'"
  connection <- runExceptT $ connect "localhost" 102
  case connection of
    Right con -> do
      x <- runExceptT (writeVal con path fc val)
      case x of
        Right _ -> putStrLn "success"
        Left err -> putStrLn $ "Error: '" ++ show err ++ "'"
    Left err -> hPutStr stderr err
