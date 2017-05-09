#!/bin/bash
hsc2hs Enums.hsc -I ./libiec61850-1.0.1/src/iec61850/inc/ -I libiec61850-1.0.1/src/common/inc/ -Ilibiec61850-1.0.1/src/hal/inc/ -Ilibiec61850-1.0.1/src/mms/inc/ -Ilibiec61850-1.0.1/src/logging/
