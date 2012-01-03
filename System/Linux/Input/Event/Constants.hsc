{-# LANGUAGE ForeignFunctionInterface #-}

module System.Linux.Input.Event.Constants where

import Foreign.C.Types
import Data.Word
import Data.Int

#include <linux/input.h>

newtype EventType = EventType Word16 deriving (Show, Eq)
#{enum EventType, EventType
 , syn         = EV_SYN
 , key         = EV_KEY
 , rel         = EV_REL
 , abs         = EV_ABS
 , msc         = EV_MSC
 , sw          = EV_SW 
 , led         = EV_LED
 , snd         = EV_SND
 , rep         = EV_REP
 , ff          = EV_FF 
 , ff_status   = EV_FF_STATUS
 , pwr         = EV_PWR
 }

newtype SyncType = SyncType Word16 deriving (Show, Eq)
#{enum SyncType, SyncType
 , report      = SYN_REPORT
 , config      = SYN_CONFIG
 , mt_report   = SYN_MT_REPORT
 , dropped     = SYN_DROPPED
 }

-- TODO Finish
newtype Key = Key Word16 deriving (Show, Eq)
#{enum Key, Key
 , reserved    = KEY_RESERVED
 , esc         = KEY_ESC
 }

newtype RelAxis = RelAxis Word16 deriving (Show, Eq)

newtype AbsAxis = AbsAxis Word16 deriving (Show, Eq)
