module Main where

import System.IO
import System.Linux.Input.Device
import System.Linux.Input.Event
import Control.Monad

main = do
    f <- openFile "/dev/input/event4" ReadMode
    getDeviceName f >>= print
    getDeviceId f >>= print
    forever $ do a <- hReadEvent f
                 print a
