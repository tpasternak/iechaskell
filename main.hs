import Data.Array
import Data.ByteString.Char8
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Int

data SIedConnection

type IedClientError= CInt

foreign import ccall unsafe "iec61850_client.h IedConnection_connect"
  c_IedConnection_connect :: Ptr SIedConnection
                             -> Ptr IedClientError
                             -> CString
                             -> CInt
                             -> IO ()

foreign import ccall unsafe "iec61850_client.h IedConnection_create"
  iedConnectionCreate :: IO(Ptr SIedConnection)

iedConnectionConnect :: Ptr SIedConnection -> String -> Int32 -> IO IedClientError
iedConnectionConnect con host port =
  alloca $ \err -> do
    useAsCString (pack "localhost") $ \str ->
      c_IedConnection_connect con err str (CInt port)
    peek err

main = do
  con <-iedConnectionCreate
  e <- iedConnectionConnect con "hostname" 102
  print e
