module MmsInternal (fromCMmsVal, SMmsValue, SMmsVariableSpecification) where

import           BitString
import           Control.Exception
import           Control.Monad
import           Data.Array
import           Data.ByteString.Char8 hiding (head, putStr, putStrLn)
import           Data.Int
import           Enums.MmsType
import           EnumsInternal
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Mms

data SMmsVariableSpecification

data SMmsValue

foreign import ccall unsafe "iec61850_client.h MmsValue_toString"
               c_MmsValue_toString :: Ptr SMmsValue -> IO CString

foreign import ccall unsafe "iec61850_client.h MmsValue_getType"
               c_MmsValue_getType :: Ptr SMmsValue -> IO CInt

foreign import ccall unsafe "iec61850_client.h MmsValue_toInt32"
               c_MmsValue_toInt32 :: Ptr SMmsValue -> IO CInt

foreign import ccall unsafe "iec61850_client.h MmsValue_toUint32"
               c_MmsValue_toUint32 :: Ptr SMmsValue -> IO CUint32

foreign import ccall unsafe "iec61850_client.h MmsValue_getBoolean"
               c_MmsValue_getBoolean :: Ptr SMmsValue -> IO CBool

foreign import ccall unsafe
               "iec61850_client.h MmsValue_getUtcTimeInMsWithUs"
               c_MmsValue_getUtcTimeInMsWithUs ::
               Ptr SMmsValue -> Ptr CUint32 -> IO CUint64

foreign import ccall unsafe
               "iec61850_client.h MmsValue_getArraySize" c_MmsValue_getArraySize
               :: Ptr SMmsValue -> IO CUint32

foreign import ccall unsafe "iec61850_client.h MmsValue_getElement"
               c_MmsValue_getElement ::
               Ptr SMmsValue -> CInt -> IO (Ptr SMmsValue)

foreign import ccall unsafe
               "iec61850_client.h MmsValue_getBitStringAsInteger"
               c_MmsValue_getBitStringAsInteger :: Ptr SMmsValue -> IO CUint32

foreign import ccall unsafe "iec61850_client.h MmsValue_toDouble"
               c_MmsValue_toDouble :: Ptr SMmsValue -> IO CDouble

fromCMmsVal mmsVal = withForeignPtr mmsVal fromCMmsValUnsafe

fromCMmsValUnsafe mmsVal = do
  type_ <- c_MmsValue_getType mmsVal
  case MmsType type_ of
    t
      | t == mms_integer -> MmsInteger . fromIntegral <$> c_MmsValue_toInt32 mmsVal
      | t == mms_boolean -> MmsBoolean . (/= cFalse) <$> c_MmsValue_getBoolean mmsVal
      | t == mms_visible_string -> MmsVisibleString <$> (c_MmsValue_toString >=> peekCString) mmsVal
      | t == mms_bit_string -> MmsBitString . BitString . fromIntegral <$> c_MmsValue_getBitStringAsInteger
                                                                             mmsVal
      | t == mms_float -> MmsFloat . realToFrac <$> c_MmsValue_toDouble mmsVal
      | t == mms_unsigned -> MmsUnsigned . fromIntegral <$> c_MmsValue_toUint32 mmsVal
      | t == mms_utc_time ->
          alloca $ \usecPtr -> do
            msec <- c_MmsValue_getUtcTimeInMsWithUs mmsVal usecPtr
            usec <- peek usecPtr
            return $ MmsUtcTime $ 1000 * fromIntegral msec + fromIntegral usec
      | t == mms_structure -> do
          size <- fromIntegral <$> c_MmsValue_getArraySize mmsVal
          MmsStructure <$> forM [0 .. (size - 1)]
                             (\idx -> do
                                elem <- c_MmsValue_getElement mmsVal idx
                                fromCMmsValUnsafe elem)

    otherwise -> return $ MmsUnknown $ show $ MmsType type_

data MmsVarSpec = MmsVarSpec { varName :: String, varType :: MmsType }
  deriving (Show)
