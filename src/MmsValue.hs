module MmsValue where

import           BitString
import           Control.Exception
import           Data.Array
import           Data.ByteString.Char8 hiding (head, putStr, putStrLn)
import           Data.Int
import           Enums
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

data SMmsVariableSpecification

data SMmsValue

foreign import ccall unsafe
               "iec61850_client.h MmsVariableSpecification_getType"
               c_MmsVariableSpecification_getType ::
               Ptr SMmsVariableSpecification -> IO CInt

foreign import ccall unsafe
               "iec61850_client.h &MmsVariableSpecification_destroy"
               c_MmsVariableSpecification_destroy ::
               FunPtr (Ptr SMmsVariableSpecification -> IO ())

foreign import ccall unsafe "iec61850_client.h MmsValue_toString"
               c_MmsValue_toString :: Ptr SMmsValue -> IO CString

foreign import ccall unsafe "iec61850_client.h MmsValue_getType"
               c_MmsValue_getType :: Ptr SMmsValue -> IO CInt

foreign import ccall unsafe "iec61850_client.h MmsValue_toInt32"
               c_MmsValue_toInt32 :: Ptr SMmsValue -> IO CInt

foreign import ccall unsafe "iec61850_client.h MmsValue_getBoolean"
               c_MmsValue_getBoolean :: Ptr SMmsValue -> IO CBool

foreign import ccall unsafe
               "iec61850_client.h MmsValue_getUtcTimeInMsWithUs"
               c_MmsValue_getUtcTimeInMsWithUs ::
               Ptr SMmsValue -> Ptr CUint32 -> IO CUint64

foreign import ccall unsafe
               "iec61850_client.h MmsValue_getBitStringAsInteger"
               c_MmsValue_getBitStringAsInteger :: Ptr SMmsValue -> IO CUint32

foreign import ccall unsafe "iec61850_client.h &MmsValue_delete"
               c_MmsValue_delete :: FunPtr (Ptr SMmsValue -> IO ())

fromCMmsVal mmsVal = do
  type_ <- withForeignPtr mmsVal c_MmsValue_getType
  case MmsType type_ of
    t
      | t == mms_integer -> MmsInteger <$> withForeignPtr mmsVal c_MmsValue_toInt32
      | t == mms_boolean -> do
          cbool <- withForeignPtr mmsVal c_MmsValue_getBoolean
          return $ MmsBoolean (cbool /= cFalse)
      | t == mms_visible_string -> do
          str <- withForeignPtr mmsVal c_MmsValue_toString
          pstr <- peekCString str
          free str
          return $ MmsVisibleString pstr
      | t == mms_utc_time ->
          alloca $ \usecPtr -> do
            msec <- withForeignPtr mmsVal (`c_MmsValue_getUtcTimeInMsWithUs` usecPtr)
            usec <- peek usecPtr
            return $ MmsUtcTime $ 1000 * fromIntegral msec + fromIntegral usec
      | t == mms_bit_string -> do
          cbitstring <- fromIntegral <$> withForeignPtr mmsVal c_MmsValue_getBitStringAsInteger
          return $ MmsBitString $ BitString cbitstring

    otherwise -> return $ MmsUnknown $ show $ MmsType type_

data MmsVarSpec = MmsVarSpec { varName :: String, varType :: MmsType }
  deriving (Show)
