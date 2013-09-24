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
 }

data RelAxis = RelX | RelY | RelZ
             | RelRX | RelRY | RelRZ
             | RelHWheel
             | RelDial
             | RelWheel
             | RelMisc
             deriving (Show, Eq, Ord)

instance Enum RelAxis where
    toEnum REL_X        = RelX
    toEnum REL_Y        = RelY
    toEnum REL_Z        = RelZ
    toEnum REL_RX       = RelRX
    toEnum REL_RY       = RelRY
    toEnum REL_RZ       = RelRZ
    toEnum REL_HWHEEL   = RelHWheel
    toEnum REL_DIAL     = RelDial
    toEnum REL_WHEEL    = RelWheel
    toEnum REL_MISC     = RelMisc
    toEnum _            = error $ "toEnum(RelAxis): Unknown relative axis type: "++show x

    fromEnum RelX         = REL_X
    fromEnum RelY         = REL_Y
    fromEnum RelZ         = REL_Z
    fromEnum RelRX        = REL_RX
    fromEnum RelRY        = REL_RY
    fromEnum RelRZ        = REL_RZ
    fromEnum RelHWheel    = REL_HWHEEL
    fromEnum RelDial      = REL_DIAL
    fromEnum RelWheel     = REL_WHEEL
    fromEnum RelMisc      = REL_MISC

data AbsAxis = AbsX | AbsY | AbsZ
             | AbsRX | AbsRY | AbsRZ
             deriving (Show, Eq, Ord)

instance Enum AbsAxis where
    toEnum ABS_X        = AbsX
    toEnum ABS_Y        = AbsY
    toEnum ABS_Z        = AbsZ
    toEnum ABS_RX       = AbsRX
    toEnum ABS_RY       = AbsRY
    toEnum ABS_RZ       = AbsRZ
    toEnum _            = error $ "toEnum(AbsAxis): Unknown relative axis type: "++show x

    fromEnum AbsX         = ABS_X
    fromEnum AbsY         = ABS_Y
    fromEnum AbsZ         = ABS_Z
    fromEnum AbsRX        = ABS_RX
    fromEnum AbsRY        = ABS_RY
    fromEnum AbsRZ        = ABS_RZ
