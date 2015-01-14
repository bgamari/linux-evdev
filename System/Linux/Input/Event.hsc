{-# LANGUAGE ForeignFunctionInterface #-}

module System.Linux.Input.Event ( -- * Events
                                  Event(..)
                                , hReadEvent
                                , KeyEventType(..)
                                , module System.Linux.Input.Event.Constants
                                ) where

import Data.Word
import Data.Int
import Data.ByteString as BS
import Data.ByteString.Internal
import Data.Time.Clock

import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types

import System.IO

import System.Linux.Input.Event.Constants

#include <linux/input.h>

-- | An Event
data Event = SyncEvent { evTimestamp :: DiffTime
                       , evSyncCode :: SyncType }
           | KeyEvent  { evTimestamp :: DiffTime
                       , evKeyCode :: Key
                       , evKeyEventType :: KeyEventType }
           | RelEvent  { evTimestamp :: DiffTime
                       , evRelAxis :: RelAxis
                       , evValue :: Int32 }
           | AbsEvent  { evTimestamp :: DiffTime
                       , evAbsAxis :: AbsAxis
                       , evValue :: Int32 }
           | MscEvent  { evTimestamp :: DiffTime }
           | SwEvent   { evTimestamp :: DiffTime }
           | LedEvent  { evTimestamp :: DiffTime }
           | SndEvent  { evTimestamp :: DiffTime }
           | RepEvent  { evTimestamp :: DiffTime }
           | FfEvent   { evTimestamp :: DiffTime }
           | FfStatusEvent { evTimestamp :: DiffTime }
           deriving (Show, Eq)

data KeyEventType = Released | Depressed | Repeated
                  deriving (Show, Eq, Ord, Enum, Bounded)

instance Storable Event where
  sizeOf _ = (#size struct input_event)
  alignment = sizeOf
  peek ptr = do let time = (#ptr struct input_event, time) ptr
                sec <- (#peek struct timeval, tv_sec) time
                usec <- (#peek struct timeval, tv_usec) time
                _type <- (#peek struct input_event, type) ptr :: IO Word16
                code <- (#peek struct input_event, code) ptr :: IO Word16
                value <- (#peek struct input_event, value) ptr :: IO Int32
                let t = 1000000000000*fromIntegral (sec::Int) + 1000000*fromIntegral (usec::Int)
                return $ case _type of
                     (#const EV_SYN)     -> SyncEvent { evTimestamp = picosecondsToDiffTime t
                                                      , evSyncCode = SyncType code
                                                      }
                     (#const EV_KEY)     -> KeyEvent { evTimestamp = picosecondsToDiffTime t
                                                     , evKeyCode = Key code
                                                     , evKeyEventType = toEnum (fromIntegral value)
                                                     }
                     (#const EV_REL)     -> RelEvent { evTimestamp = picosecondsToDiffTime t
                                                     , evRelAxis = RelAxis code
                                                     , evValue = value
                                                     }
                     (#const EV_ABS)     -> AbsEvent { evTimestamp = picosecondsToDiffTime t
                                                     , evAbsAxis = AbsAxis code
                                                     , evValue = value
                                                     }
                     (#const EV_MSC)     -> MscEvent { evTimestamp = picosecondsToDiffTime t }
                     (#const EV_SW )     -> SwEvent { evTimestamp = picosecondsToDiffTime t }
                     (#const EV_LED)     -> LedEvent { evTimestamp = picosecondsToDiffTime t }
                     (#const EV_SND)     -> SndEvent { evTimestamp = picosecondsToDiffTime t }
                     (#const EV_REP)     -> RepEvent { evTimestamp = picosecondsToDiffTime t }
                     (#const EV_FF)      -> FfEvent { evTimestamp = picosecondsToDiffTime t }
                     (#const EV_FF_STATUS) -> FfStatusEvent { evTimestamp = picosecondsToDiffTime t }
                     otherwise  -> error $ "unknown event type: " ++ show _type

  poke = error "Storable(System.Linux.Input.Event): poke not supported"

-- | Read an event
hReadEvent :: Handle -> IO (Maybe Event)
hReadEvent h = do
    a <- hGet h $ sizeOf (undefined::Event)
    case a of
         _ | BS.null a  -> return Nothing
         _ | otherwise  -> getEvent a >>= return . Just

getEvent :: ByteString -> IO Event
getEvent bs = do
    let (fptr, off, len) = toForeignPtr bs
    withForeignPtr fptr $ peek . castPtr 

