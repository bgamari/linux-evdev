{-# LANGUAGE ForeignFunctionInterface #-}

module System.Linux.Input.Event ( Event(..)
                                , hReadEvent
                                ) where

import Data.ByteString as BS
import Data.ByteString.Internal
import Data.Time.Clock
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO

#include <linux/input.h>

data Event = Event { evTimestamp :: DiffTime
                   , evType :: Int
                   , evCode :: Int
                   , evValue :: Int
                   }
             deriving (Show, Eq)

instance Storable Event where
  sizeOf _ = (#size struct input_event)
  alignment = sizeOf
  peek ptr = do let time = (#ptr struct input_event, time) ptr
                sec <- (#peek struct timeval, tv_sec) time
                usec <- (#peek struct timeval, tv_usec) time
                _type <- (#peek struct input_event, type) ptr
                code <- (#peek struct input_event, code) ptr
                value <- (#peek struct input_event, value) ptr
                let t = 1000000000000*fromIntegral (sec::Int) + 1000000*fromIntegral (usec::Int)
                return Event { evTimestamp = picosecondsToDiffTime t
                             , evType = _type
                             , evCode = code
                             , evValue = value
                             }

  poke ptr (Event { evTimestamp=ts, evType=t, evCode=c, evValue=v }) =
    do let time = (#ptr struct input_event, time) ptr
           sec  = truncate (realToFrac ts :: Double) :: Int
           usec = truncate (realToFrac ts / 1e-6 :: Double) :: Int
       (#poke struct timeval, tv_sec) ptr sec
       (#poke struct timeval, tv_usec) ptr usec
       (#poke struct input_event, type) ptr t
       (#poke struct input_event, code) ptr c
       (#poke struct input_event, value) ptr v

hReadEvent :: Handle -> IO (Maybe Event)
hReadEvent h =
  do a <- hGet h $ sizeOf (undefined::Event)
     case a of
          _ | BS.null a  -> return Nothing
          _ | otherwise  -> getEvent a >>= return . Just

getEvent :: ByteString -> IO Event
getEvent bs =
  do let (fptr, off, len) = toForeignPtr bs
     withForeignPtr fptr $ peek . castPtr . (flip plusPtr) off

