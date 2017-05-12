module Iec61850 (logicalNodeDirectory, SIedConnection, connect, logicalDevices,
                 logicalNodes, logicalNodeVariables, dataObjectDirectory) where

import           Data.Array
import           Data.ByteString.Char8 hiding (head,putStr, putStrLn)
import           Data.Int
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.Storable
import           Enums
import           Control.Exception

data SIedConnection
data SLinkedList

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
               "iec61850_client.h &IedConnection_destroy" c_IedConnection_destroy
               :: FunPtr (Ptr SIedConnection -> IO ())

foreign import ccall unsafe "iec61850_client.h IedConnection_getLogicalDeviceList"
   c_IedConnection_getLogicalDeviceList :: Ptr SIedConnection -> Ptr IedClientError -> IO(Ptr SLinkedList)

foreign import ccall unsafe "iec61850_client.h IedConnection_getLogicalDeviceDirectory"
   c_IedConnection_getLogicalDeviceDirectory :: Ptr SIedConnection -> Ptr IedClientError -> CString -> IO(Ptr SLinkedList)

foreign import ccall unsafe "iec61850_client.h IedConnection_getLogicalNodeVariables"
   c_IedConnection_getLogicalNodeVariables :: Ptr SIedConnection -> Ptr IedClientError -> CString -> IO(Ptr SLinkedList)

foreign import ccall unsafe "iec61850_client.h IedConnection_getLogicalNodeDirectory"
   c_IedConnection_getLogicalNodeDirectory :: Ptr SIedConnection -> Ptr IedClientError -> CString -> CInt -> IO(Ptr SLinkedList)

foreign import ccall unsafe "iec61850_client.h IedConnection_getDataDirectory"
   c_IedConnection_getDataDirectory :: Ptr SIedConnection -> Ptr IedClientError -> CString -> CInt -> IO(Ptr SLinkedList)

foreign import ccall unsafe "iec61850_client.h LinkedList_getData"
   c_LinkedList_getData :: Ptr SLinkedList -> IO(Ptr ())

foreign import ccall unsafe "iec61850_client.h LinkedList_getNext"
   c_LinkedList_getNext :: Ptr SLinkedList -> IO(Ptr SLinkedList)

foreign import ccall unsafe "iec61850_client.h LinkedList_destroy"
   c_LinkedList_destroy :: Ptr SLinkedList -> IO ()



connect :: String -> Int32 -> IO (ForeignPtr SIedConnection)
connect host port = do
  rawCon <- c_IedConnectionCreate
  con <- newForeignPtr c_IedConnection_destroy rawCon
  e <- iedConnectionConnect rawCon host 102
  case e of
    0 -> return con
    _ -> throwIO (IedConnectionException e)
  where
    iedConnectionConnect :: Ptr SIedConnection -> String -> Int32 -> IO IedClientError
    iedConnectionConnect con host port =
      alloca $ \err -> do
        useAsCString (pack host) $ \str -> c_IedConnection_connect con err str (CInt port)
        peek err

close con = withForeignPtr con c_IedConnection_close

linkedListgetString :: Ptr SLinkedList -> IO String
linkedListgetString list = do
    val <- c_LinkedList_getData list
    let valStr = castPtr val
    peekCString valStr


linkedListToList :: Ptr SLinkedList -> [String]-> IO [String]
linkedListToList list acc = do
  next <- c_LinkedList_getNext list
  if next == nullPtr then
    return acc
  else do
    str <- linkedListgetString next
    linkedListToList next (str:acc)

logicalDevices :: ForeignPtr SIedConnection -> IO [String]
logicalDevices con =
      alloca $ \err -> do
        linkedList <- withForeignPtr con (`c_IedConnection_getLogicalDeviceList` err)
        errNo <- peek err
        case errNo of
          0 -> do
            ans <- linkedListToList linkedList []
            c_LinkedList_destroy linkedList
            return ans
          _ -> throwIO (IedConnectionException errNo)

logicalNodes :: ForeignPtr SIedConnection -> String -> IO [String]
logicalNodes con device =
  alloca $ \err ->
    useAsCString (pack device) $ \dev -> do
      nodes <- withForeignPtr con (\rawCon -> c_IedConnection_getLogicalDeviceDirectory rawCon err dev)
      errNo <- peek err
      case errNo of
          0 -> do
            ans <- linkedListToList nodes []
            c_LinkedList_destroy nodes
            return ans
          _ -> throwIO (IedConnectionException errNo)

logicalNodeVariables :: ForeignPtr SIedConnection -> String -> IO [String]
logicalNodeVariables con lnode =
  alloca $ \err ->
      useAsCString (pack lnode) $ \dev -> do
      nodes <- withForeignPtr con (\rawCon -> c_IedConnection_getLogicalNodeVariables rawCon err dev)
      errNo <- peek err
      case errNo of
          0 -> do
            ans <- linkedListToList nodes []
            c_LinkedList_destroy nodes
            return ans
          _ -> throwIO (IedConnectionException errNo)

logicalNodeDirectory :: ForeignPtr SIedConnection -> String -> AcsiClass ->IO [String]
logicalNodeDirectory con lnode acsiClass =
    alloca $ \err ->
      useAsCString (pack lnode) $ \dev -> do
        nodes <- withForeignPtr con (\rawCon -> c_IedConnection_getLogicalNodeDirectory rawCon err dev (unAcsiClass acsiClass))
        errNo <- peek err
        case errNo of
          0 -> do
            ans <- linkedListToList nodes []
            c_LinkedList_destroy nodes
            return ans
          _ -> throwIO (IedConnectionException errNo)

dataObjectDirectory :: ForeignPtr SIedConnection -> String -> AcsiClass ->IO [String]
dataObjectDirectory con lnode acsiClass =
    alloca $ \err ->
      useAsCString (pack lnode) $ \dev -> do
        nodes <- withForeignPtr con (\rawCon -> c_IedConnection_getDataDirectory rawCon err dev (unAcsiClass acsiClass))
        errNo <- peek err
        case errNo of
          0 -> do
            ans <- linkedListToList nodes []
            c_LinkedList_destroy nodes
            return ans
          _ -> throwIO (IedConnectionException errNo)


data IedConnectionException = IedConnectionException CInt
  deriving (Show)

instance Exception IedConnectionException
