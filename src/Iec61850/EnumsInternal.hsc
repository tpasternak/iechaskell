module Iec61850.EnumsInternal where

import Data.Word
import Data.Int

#include "iec61850_client.h"

type CBool = #{type bool}

cFalse, cTrue :: CBool
cFalse = 0
cTrue = 1

type CUint64 = #{type uint64_t}

type CUint32 = #{type uint32_t}

type CInt32 = #{type int32_t}
