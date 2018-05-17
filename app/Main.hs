module Main where

import Log
import LogAnalysis

main :: IO ()
main = do
  let x = "sfasf"
  print $ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
  print $ parseMessage "I 29 la la la"  == LogMessage Info 29 "la la la"
  print $ parseMessage "This is not in the right format" == Unknown "This is not in the right format"
  msgs <- testParse parse 40 "error.log"
  print msgs
