module Iec61850.Client (
    connect,
    logicalDevices,
    logicalNodeDirectory,
    logicalNodes,
    logicalNodeVariables,
    dataObjectDirectoryByFC,
    readVal,
    mmsType,
    discover,
    ) where

import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8    (pack, useAsCString)
import           Data.Int
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Iec61850.Enums.AcsiClass
import           Iec61850.Enums.FC
import           Iec61850.Enums.MmsType
import           Iec61850.LinkedList
import           Iec61850.Mms
import           Iec61850.MmsInternal
import           Data.List.Utils

data SIedConnection

type IedConnection = ForeignPtr SIedConnection

type IedClientError = CInt

data IedConnectionException = IedConnectionException CInt
  deriving (Show)

instance Exception IedConnectionException

foreign import ccall unsafe
               "iec61850_client.h MmsVariableSpecification_getType"
               c_MmsVariableSpecification_getType ::
               Ptr SMmsVariableSpecification -> IO CInt

foreign import ccall unsafe
               "iec61850_client.h &MmsVariableSpecification_destroy"
               c_MmsVariableSpecification_destroy ::
               FunPtr (Ptr SMmsVariableSpecification -> IO ())

foreign import ccall unsafe "iec61850_client.h &MmsValue_delete"
               c_MmsValue_delete :: FunPtr (Ptr SMmsValue -> IO ())

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
               "iec61850_client.h IedConnection_getDataDirectoryByFC"
               c_IedConnection_getDataDirectoryByFC ::
               Ptr SIedConnection ->
                 Ptr IedClientError -> CString -> CInt -> IO (Ptr SLinkedList)

-- | Connect to an IED of the specified IP address and port
connect :: String -- ^ IP address
        -> Int32 -- ^ Port
        -> IO IedConnection -- ^ IED connection handle
connect host port = do
  rawCon <- c_IedConnectionCreate
  con <- newForeignPtr c_IedConnection_destroy rawCon
  e <- iedConnectionConnect rawCon host port
  case e of
    0 -> return con
    _ -> throwIO (IedConnectionException e)

  where
    iedConnectionConnect :: Ptr SIedConnection -> String -> Int32 -> IO IedClientError
    iedConnectionConnect con host port =
      alloca $ \err -> do
        useAsCString (pack host) $ \str -> c_IedConnection_connect con err str (CInt port)
        peek err

getStringListFromIed con fun =
  alloca $ \err -> do
    nodes <- withForeignPtr con (fun err)
    nodesSafe <- newForeignPtr c_LinkedList_destroy nodes
    errNo <- peek err
    case errNo of
      0 -> linkedListToList nodesSafe
      _ -> throwIO (IedConnectionException errNo)

-- | Get list of all logical nodes from the IED
logicalDevices :: IedConnection -> IO [String]
logicalDevices con =
  getStringListFromIed con (flip c_IedConnection_getLogicalDeviceList)

logicalNodes :: IedConnection -> String -> IO [String]
logicalNodes con device =
  useAsCString (pack device) $ \dev ->
    getStringListFromIed con
      (\err rawCon -> c_IedConnection_getLogicalDeviceDirectory rawCon err dev)

logicalNodeVariables :: IedConnection -> String -> IO [String]
logicalNodeVariables con lnode =
  useAsCString (pack lnode) $ \dev ->
    getStringListFromIed con (\err rawCon -> c_IedConnection_getLogicalNodeVariables rawCon err dev)

logicalNodeDirectory :: IedConnection -> String -> AcsiClass -> IO [String]
logicalNodeDirectory con lnode acsiClass =
  useAsCString (pack lnode) $ \dev ->
    getStringListFromIed con
      (\err rawCon -> c_IedConnection_getLogicalNodeDirectory rawCon err dev (unAcsiClass acsiClass))

dataObjectDirectoryByFC :: IedConnection -> String -> FunctionalConstraint -> IO [String]
dataObjectDirectoryByFC con lnode fc =
  useAsCString (pack lnode) $ \dev ->
    getStringListFromIed con
      (\err rawCon ->
         c_IedConnection_getDataDirectoryByFC rawCon err dev (unFunctionalConstraint fc))

readVal :: IedConnection -> String -> FunctionalConstraint -> IO MmsVar
readVal con daReference fc =
  useAsCString (pack daReference) $ \p -> alloca $ \err -> do
    mmsVal <- withForeignPtr con (\rawCon -> c_IedConnection_readObject rawCon err p (unFunctionalConstraint fc))
    safeMmsVal <- newForeignPtr c_MmsValue_delete mmsVal
    fromCMmsVal safeMmsVal

mmsSpec :: IedConnection -> String -> FunctionalConstraint -> IO (ForeignPtr SMmsVariableSpecification)
mmsSpec con path fc =
  useAsCString (pack path) $ \p -> alloca $ \err -> do
    mmsSpec <- withForeignPtr con (\rawCon -> c_IedConnection_getVariableSpecification rawCon err p (unFunctionalConstraint fc))
    fMmsSpec <- newForeignPtr c_MmsVariableSpecification_destroy mmsSpec
    errNo <- peek err
    case errNo of
      0 -> return fMmsSpec
      _ -> throwIO (IedConnectionException errNo)

mmsType :: IedConnection -> String -> FunctionalConstraint -> IO MmsType
mmsType con path fc = do
  fMmsSpec <- mmsSpec con path fc
  MmsType <$> withForeignPtr fMmsSpec c_MmsVariableSpecification_getType

discover :: IedConnection -> IO [(String, FunctionalConstraint)]
discover con = do
  ldevices <- logicalDevices con
  liftM msum $ forM ldevices $
    \dev -> do
      nodes <- logicalNodes con dev
      liftM msum $ forM nodes $ \node-> do
        let nodeRef = dev  ++ "/" ++ node
        lnVars <- logicalNodeVariables con nodeRef
        let lnVarsWithoutFC = filter ((>=3).length) lnVars
        forM lnVarsWithoutFC $ \var -> do
           let fc = readFC (take 2 var)
           let varPath =  nodeRef ++ "." ++ (drop 3 var)
           return (replace "$" "." varPath, fc)

