import Data.Array
import Data.ByteString.Char8
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable

data SIedConnection 

type IedClientError= CInt

-- 	IedConnection_connect (IedConnection self, IedClientError *error, const char *hostname, int tcpPort)

foreign import ccall unsafe "iec61850_client.h IedConnection_connect"
  c_IedConnection_connect :: Ptr SIedConnection
                             -> Ptr IedClientError
                             -> CString
                             -> CInt
                             -> IO ()

foreign import ccall unsafe "iec61850_client.h IedConnection_create"
  iedConnectionCreate :: IO(Ptr SIedConnection)


main = do
  con <-iedConnectionCreate
  alloca $ \err -> do
    useAsCString (pack "localhost") $ \str -> do
      c_IedConnection_connect con err str (CInt 102)
    e <- peek err
    print e
