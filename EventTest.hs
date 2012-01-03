module Main where

import System.IO
import System.Linux.Input.Event

main =
  do f <- openFile "/dev/input/event5" ReadMode
     a <- hReadEvent f
     print a
