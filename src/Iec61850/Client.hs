
module Iec61850.Client (
    connect,
    logicalDevices,
    logicalNodeDirectory,
    logicalNodes,
    logicalNodeVariables,
    dataObjectDirectoryByFC,
    readVal,
    writeVal,
    mmsType,
    discover,
    ) where

import           Control.Exception
import           Data.Either.Utils(fromRight)
import           Control.Monad
import           Control.Monad.Except
import           Data.ByteString.Char8    (pack, useAsCString)
import           Data.Int
import           Data.List.Utils
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
import           Iec61850.NameTree

type LengthMonad = ExceptT String IO

data SIedConnection

type IedConnection = ForeignPtr SIedConnection

type IedClientError = CInt

newtype IedConnectionException = IedConnectionException CInt
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

foreign import ccall unsafe
               "iec61850_client.h IedConnection_writeObject"
               c_IedConnection_writeObject :: Ptr SIedConnection -> Ptr IedClientError -> CString -> CInt -> Ptr SMmsValue -> IO ()

-- | Connect to an IED of the specified IP address and port
connect
  :: String -- ^ IP address
  -> Int32 -- ^ Port
  -> LengthMonad IedConnection -- ^ IED connection handle
connect host port = do
  rawCon <- liftIO $ c_IedConnectionCreate
  con    <- liftIO $ newForeignPtr c_IedConnection_destroy rawCon
  e      <- liftIO $ iedConnectionConnect rawCon host port
  case e of
    0 -> return con
    _ -> throwError (show e)
 where
  iedConnectionConnect
    :: Ptr SIedConnection -> String -> Int32 -> IO IedClientError
  iedConnectionConnect con host port = alloca $ \err -> do
    useAsCString (pack host)
      $ \str -> c_IedConnection_connect con err str (CInt port)
    peek err

-- | Get list of all logical nodes from the IED
logicalDevices :: IedConnection -> IO (Either String [String])
logicalDevices con = alloca $ \err -> do
  nodes <- withForeignPtr con ((flip c_IedConnection_getLogicalDeviceList) err)
  nodesSafe <- newForeignPtr c_LinkedList_destroy nodes
  errNo <- peek err
  case errNo of
    0 -> do
      x <- linkedListToList nodesSafe
      return $ Right x
    _ -> return $ Left $ show errNo

logicalNodes :: IedConnection -> String -> IO (Either String [String])
logicalNodes con device =
  useAsCString (pack device) $ \dev -> alloca $ \err -> do
    nodes <- withForeignPtr
      con
      ( ( \err rawCon ->
          c_IedConnection_getLogicalDeviceDirectory rawCon err dev
        )
        err
      )
    nodesSafe <- newForeignPtr c_LinkedList_destroy nodes
    errNo     <- peek err
    case errNo of
      0 -> do
        x <- linkedListToList nodesSafe
        return $ Right x
      _ -> return $ Left $ show errNo

logicalNodeVariables :: IedConnection -> String -> IO (Either String [String])
logicalNodeVariables con lnode =
  useAsCString (pack lnode) $ \dev -> alloca $ \err -> do
    nodes <- withForeignPtr
      con
      ( (\err rawCon -> c_IedConnection_getLogicalNodeVariables rawCon err dev)
        err
      )
    nodesSafe <- newForeignPtr c_LinkedList_destroy nodes
    errNo     <- peek err
    case errNo of
      0 -> do
        x <- linkedListToList nodesSafe
        return $ Right x
      _ -> return $ Left $ show errNo

logicalNodeDirectory
  :: IedConnection -> String -> AcsiClass -> IO (Either String [String])
logicalNodeDirectory con lnode acsiClass =
  useAsCString (pack lnode) $ \dev -> alloca $ \err -> do
    nodes <- withForeignPtr
      con
      ( ( \err rawCon -> c_IedConnection_getLogicalNodeDirectory
          rawCon
          err
          dev
          (unAcsiClass acsiClass)
        )
        err
      )
    nodesSafe <- newForeignPtr c_LinkedList_destroy nodes
    errNo     <- peek err
    case errNo of
      0 -> do
        x <- linkedListToList nodesSafe
        return $ Right x
      _ -> return $ Left $ show errNo

dataObjectDirectoryByFC
  :: IedConnection
  -> String
  -> FunctionalConstraint
  -> IO (Either String [String])
dataObjectDirectoryByFC con lnode fc =
  useAsCString (pack lnode) $ \dev -> alloca $ \err -> do
    nodes <- withForeignPtr
      con
      ( ( \err rawCon ->
          c_IedConnection_getDataDirectoryByFC rawCon err dev (toInt fc)
        )
        err
      )
    nodesSafe <- newForeignPtr c_LinkedList_destroy nodes
    errNo     <- peek err
    case errNo of
      0 -> do
        x <- linkedListToList nodesSafe
        return $ Right x
      _ -> return $ Left $ show errNo

readVal :: IedConnection -> String -> FunctionalConstraint -> IO MmsVar
readVal con daReference fc =
  useAsCString (pack daReference) $ \p -> alloca $ \err -> do
    mmsVal <- withForeignPtr
      con
      (\rawCon -> c_IedConnection_readObject rawCon err p (toInt fc))
    safeMmsVal <- newForeignPtr c_MmsValue_delete mmsVal
    fromCMmsVal safeMmsVal

mmsSpec
  :: IedConnection
  -> String
  -> FunctionalConstraint
  -> IO (ForeignPtr SMmsVariableSpecification)
mmsSpec con path fc = useAsCString (pack path) $ \p -> alloca $ \err -> do
  mmsSpec <- withForeignPtr
    con
    ( \rawCon ->
      c_IedConnection_getVariableSpecification rawCon err p (toInt fc)
    )
  fMmsSpec <- newForeignPtr c_MmsVariableSpecification_destroy mmsSpec
  errNo    <- peek err
  case errNo of
    0 -> return fMmsSpec
    _ -> throwIO (IedConnectionException errNo)

mmsType :: IedConnection -> String -> FunctionalConstraint -> IO MmsType
mmsType con path fc = do
  fMmsSpec <- mmsSpec con path fc
  MmsType <$> withForeignPtr fMmsSpec c_MmsVariableSpecification_getType

discover :: IedConnection -> IO [(String, FunctionalConstraint)]
discover con = do
  ldevices <- fromRight <$> logicalDevices con
  fmap msum $ forM ldevices $ \dev -> do
    nodes <- fromRight <$> logicalNodes con dev
    fmap msum $ forM nodes $ \node -> do
      let nodeRef = dev ++ "/" ++ node
      lnVars <- fromRight <$> logicalNodeVariables con nodeRef
      let nameTree = buildNameTree lnVars
      let leaves   = leavesPaths nameTree
      return $ fmap (varNameToIdentityPair nodeRef) leaves
 where
  varNameToIdentityPair logicalNodeRef varName =
    let fc      = readFC (take 2 varName)
        varPath = logicalNodeRef ++ "." ++ drop 3 varName
    in  (replace "$" "." varPath, fc)

writeVal
  :: IedConnection
  -> String
  -> FunctionalConstraint
  -> MmsVar
  -> IO (Either String ())
writeVal con daReference fc v =
  useAsCString (pack daReference) $ \p -> alloca $ \err -> do
    rawVal <- toSMmsValue v
    withForeignPtr rawVal $ \x -> do
      withForeignPtr con
        $ \rawCon -> c_IedConnection_writeObject rawCon err p (toInt fc) x
      errNo <- peek err
      case errNo of
        0 -> return $ Right ()
        _ -> return $ Left $ "Error while writing: '" ++ daReference ++ "'"
