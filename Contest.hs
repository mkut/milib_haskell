module Milib.Contest
   ( contestMainOnce

   , printerYN
   ) where

import IO
import Milib.IO

contestMainOnce printer solver parser = printer stdout . solver =<< parser stdin

printerYN h = hPutStrLn h . showYN

-- vim: set expandtab:

