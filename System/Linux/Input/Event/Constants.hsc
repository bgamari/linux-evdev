{-# LANGUAGE ForeignFunctionInterface #-}

module System.Linux.Input.Event.Constants where

import Foreign.C.Types
import Data.Word
import Data.Int

#include <linux/input.h>

newtype SyncType = SyncType Word16 deriving (Show, Eq)
#{enum SyncType, SyncType
 , sync_report      = SYN_REPORT
 , sync_config      = SYN_CONFIG
 , sync_mt_report   = SYN_MT_REPORT
 , sync_dropped     = SYN_DROPPED
 }

-- TODO Finish
newtype Key = Key Word16 deriving (Show, Eq)
#{enum Key, Key
 , key_reserved    = KEY_RESERVED
 , key_esc         = KEY_ESC

 , btn_0           = BTN_0
 , btn_1           = BTN_1
 , btn_2           = BTN_2
 , btn_3           = BTN_3
 , btn_4           = BTN_4
 , btn_5           = BTN_5
 , btn_6           = BTN_6
 , btn_7           = BTN_7
 , btn_8           = BTN_8
 , btn_9           = BTN_9
 }

newtype RelAxis = RelAxis Word16 deriving (Show, Eq)
#{enum RelAxis, RelAxis
 , rel_x            = REL_X
 , rel_y            = REL_Y
 , rel_z            = REL_Z
 , rel_rx           = REL_RX
 , rel_ry           = REL_RY
 , rel_rz           = REL_RZ
 , rel_hWheel       = REL_HWHEEL
 , rel_dial         = REL_DIAL
 , rel_wheel        = REL_WHEEL
 , rel_misc         = REL_MISC
 }

newtype AbsAxis = AbsAxis Word16 deriving (Show, Eq)
#{enum AbsAxis, AbsAxis
 , abs_x            = ABS_X
 , abs_y            = ABS_Y
 , abs_z            = ABS_Z
 , abs_rx           = ABS_RX
 , abs_ry           = ABS_RY
 , abs_rz           = ABS_RZ
 }
