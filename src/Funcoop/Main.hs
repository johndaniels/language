module Funcoop.Main

import System.IO (readFile)
import Parser

fileName = "test.fnp"

main = readFile fileName >>= compile fileName
    
    
