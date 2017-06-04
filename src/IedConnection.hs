module IedConnection (
    logicalNodeDirectory,
    SIedConnection,
    connect,
    logicalDevices,
    logicalNodes,
    logicalNodeVariables,
    dataObjectDirectory,
    dataObjectDirectoryByFC,
    readVal,
    mmsType,
    ) where

import           Control.Exception
import           Data.ByteString.Char8 hiding (head, putStr, putStrLn)
import           Data.Int
import           Enums
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           LinkedList
import           MmsValue

data SIedConnection

type IedClientError = CInt

data IedConnectionException = IedConnectionException CInt
  deriving (Show)

instance Exception IedConnectionException

foreign import ccall unsafe
               "iec61850_client.h IedConnection_getVariableSpecification"
               c_IedConnection_getVariableSpecification ::
               Ptr SIedConnection ->
                 Ptr IedClientError ->
                   CString -> CInt -> IO (Ptr SMmsVariableSpecification)

foreign import ccall unsafe
               "iec61850_client.h IedConnection_readObject"
               c_IedConnection_readObject ::
               Ptr SIedConnection ->
                 Ptr CInt -> CString -> CInt -> IO (Ptr SMmsValue)

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

foreign import ccall unsafe
               "iec61850_client.h IedConnection_getLogicalDeviceList"
               c_IedConnection_getLogicalDeviceList ::
               Ptr SIedConnection -> Ptr IedClientError -> IO (Ptr SLinkedList)

foreign import ccall unsafe
               "iec61850_client.h IedConnection_getLogicalDeviceDirectory"
               c_IedConnection_getLogicalDeviceDirectory ::
               Ptr SIedConnection ->
                 Ptr IedClientError -> CString -> IO (Ptr SLinkedList)

foreign import ccall unsafe
               "iec61850_client.h IedConnection_getLogicalNodeVariables"
               c_IedConnection_getLogicalNodeVariables ::
               Ptr SIedConnection ->
                 Ptr IedClientError -> CString -> IO (Ptr SLinkedList)

foreign import ccall unsafe
               "iec61850_client.h IedConnection_getLogicalNodeDirectory"
               c_IedConnection_getLogicalNodeDirectory ::
               Ptr SIedConnection ->
                 Ptr IedClientError -> CString -> CInt -> IO (Ptr SLinkedList)

foreign import ccall unsafe
               "iec61850_client.h IedConnection_getDataDirectory"
               c_IedConnection_getDataDirectory ::
               Ptr SIedConnection ->
                 Ptr IedClientError -> CString -> IO (Ptr SLinkedList)

foreign import ccall unsafe
               "iec61850_client.h IedConnection_getDataDirectoryByFC"
               c_IedConnection_getDataDirectoryByFC ::
               Ptr SIedConnection ->
                 Ptr IedClientError -> CString -> CInt -> IO (Ptr SLinkedList)

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
  alloca $ \err -> useAsCString (pack device) $ \dev -> do
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
  alloca $ \err -> useAsCString (pack lnode) $ \dev -> do
    nodes <- withForeignPtr con (\rawCon -> c_IedConnection_getLogicalNodeVariables rawCon err dev)
    errNo <- peek err
    case errNo of
      0 -> do
        ans <- linkedListToList nodes []
        c_LinkedList_destroy nodes
        return ans
      _ -> throwIO (IedConnectionException errNo)

logicalNodeDirectory :: ForeignPtr SIedConnection -> String -> AcsiClass -> IO [String]
logicalNodeDirectory con lnode acsiClass =
  alloca $ \err -> useAsCString (pack lnode) $ \dev -> do
    nodes <- withForeignPtr con (\rawCon -> c_IedConnection_getLogicalNodeDirectory rawCon err dev (unAcsiClass acsiClass))
    errNo <- peek err
    case errNo of
      0 -> do
        ans <- linkedListToList nodes []
        c_LinkedList_destroy nodes
        return ans
      _ -> throwIO (IedConnectionException errNo)

dataObjectDirectory :: ForeignPtr SIedConnection -> String -> IO [String]
dataObjectDirectory con lnode =
  alloca $ \err -> useAsCString (pack lnode) $ \dev -> do
    nodes <- withForeignPtr con (\rawCon -> c_IedConnection_getDataDirectory rawCon err dev)
    errNo <- peek err
    case errNo of
      0 -> do
        ans <- linkedListToList nodes []
        c_LinkedList_destroy nodes
        return ans
      _ -> throwIO (IedConnectionException errNo)

dataObjectDirectoryByFC :: ForeignPtr SIedConnection -> String -> FunctionalConstraint -> IO [String]
dataObjectDirectoryByFC con lnode fc =
  alloca $ \err -> useAsCString (pack lnode) $ \dev -> do
    nodes <- withForeignPtr con (\rawCon -> c_IedConnection_getDataDirectoryByFC rawCon err dev (unFunctionalConstraint fc))
    errNo <- peek err
    case errNo of
      0 -> do
        ans <- linkedListToList nodes []
        c_LinkedList_destroy nodes
        return ans
      _ -> throwIO (IedConnectionException errNo)

readVal :: ForeignPtr SIedConnection -> String -> FunctionalConstraint -> IO MmsVar
readVal con daReference fc =
  alloca $ \err -> useAsCString (pack daReference) $ \p -> do
    mmsVal <- withForeignPtr con (\rawCon -> c_IedConnection_readObject rawCon err p (unFunctionalConstraint fc))
    safeMmsVal <- newForeignPtr c_MmsValue_delete mmsVal
    fromCMmsVal safeMmsVal

mmsSpec :: ForeignPtr SIedConnection -> String -> FunctionalConstraint -> IO (ForeignPtr SMmsVariableSpecification)
mmsSpec con path fc =
  alloca $ \err -> useAsCString (pack path) $ \p -> do
    mmsSpec <- withForeignPtr con (\rawCon -> c_IedConnection_getVariableSpecification rawCon err p (unFunctionalConstraint fc))
    fMmsSpec <- newForeignPtr c_MmsVariableSpecification_destroy mmsSpec
    errNo <- peek err
    case errNo of
      0 -> return fMmsSpec
      _ -> throwIO (IedConnectionException errNo)

mmsType :: ForeignPtr SIedConnection -> String -> FunctionalConstraint -> IO MmsType
mmsType con path fc = do
  fMmsSpec <- mmsSpec con path fc
  MmsType <$> withForeignPtr fMmsSpec c_MmsVariableSpecification_getType
