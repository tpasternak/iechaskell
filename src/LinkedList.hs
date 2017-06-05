module LinkedList where

import           Control.Exception
import           Data.Array
import           Data.ByteString.Char8 hiding (head, putStr, putStrLn)
import           Data.Int
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

linkedListToListUnsafe :: Ptr SLinkedList -> [String] -> IO [String]
linkedListToListUnsafe list acc = do
  next <- c_LinkedList_getNext list
  if next == nullPtr
    then return acc
    else do
      str <- linkedListgetString next
      linkedListToListUnsafe next (str : acc)

linkedListToList :: ForeignPtr SLinkedList -> IO [String]
linkedListToList list = withForeignPtr list (\l -> linkedListToListUnsafe l [])
