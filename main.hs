import           Data.Array
import           Data.ByteString.Char8 hiding (putStr)
import           Data.Int
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

data SIedConnection

type IedClientError = CInt

foreign import ccall unsafe
               "iec61850_client.h IedConnection_connect" c_IedConnection_connect
               ::
               Ptr SIedConnection ->
                 Ptr IedClientError -> CString -> CInt -> IO ()

foreign import ccall unsafe
               "iec61850_client.h IedConnection_create" c_IedConnectionCreate ::
               IO (Ptr SIedConnection)

foreign import ccall unsafe "iec61850_client.h IedConnection_close"
               c_IedConnection_close :: Ptr SIedConnection -> IO ()

foreign import ccall unsafe
               "iec61850_client.h IedConnection_destroy" c_IedConnection_destroy
               :: Ptr SIedConnection -> IO ()

iedConnectionConnect :: Ptr SIedConnection -> String -> Int32 -> IO IedClientError
iedConnectionConnect con host port =
  alloca $ \err -> do
    useAsCString (pack host) $ \str -> c_IedConnection_connect con err str (CInt port)
    peek err

connect :: String -> Int32 -> IO (Either IedClientError (Ptr SIedConnection))
connect host port = do
  con <- c_IedConnectionCreate
  e <- iedConnectionConnect con host 102
  case e of
    0 -> return (Right con)
    otherwise -> do
      c_IedConnection_destroy con
      return (Left e)

destroy s = do
  c_IedConnection_close s
  c_IedConnection_destroy s

main = do
  con <- connect "localhost" 102
  case con of
    Left e -> putStr "Failure: " >> print e
    Right con -> do
      destroy con
      print "Success"
