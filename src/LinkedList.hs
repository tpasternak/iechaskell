module LinkedList where

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

data SLinkedList

foreign import ccall unsafe "iec61850_client.h LinkedList_getData"
               c_LinkedList_getData :: Ptr SLinkedList -> IO (Ptr ())

foreign import ccall unsafe "iec61850_client.h LinkedList_getNext"
               c_LinkedList_getNext :: Ptr SLinkedList -> IO (Ptr SLinkedList)

foreign import ccall unsafe "iec61850_client.h &LinkedList_destroy"
               c_LinkedList_destroy :: FunPtr (Ptr SLinkedList -> IO ())

linkedListgetString :: Ptr SLinkedList -> IO String
linkedListgetString list = do
  val <- c_LinkedList_getData list
  let valStr = castPtr val
  peekCString valStr

linkedListToList :: Ptr SLinkedList -> [String] -> IO [String]
linkedListToList list acc = do
  next <- c_LinkedList_getNext list
  if next == nullPtr
    then return acc
    else do
      str <- linkedListgetString next
      linkedListToList next (str : acc)

linkedListToListSafe :: ForeignPtr SLinkedList -> IO [String]
linkedListToListSafe list = withForeignPtr list (\l -> linkedListToList l [])
