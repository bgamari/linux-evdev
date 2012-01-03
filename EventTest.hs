module Main where

import System.IO
import System.Linux.Input.Event
import Foreign.Storable
import Control.Monad

main =
  do f <- openFile "/dev/input/event4" ReadMode
     print (sizeOf (undefined::Event))
     forever $ do a <- hReadEvent f
                  print a
