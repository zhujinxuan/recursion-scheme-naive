module Lib
    ( someFunc
    ) where

import qualified List as L

someFunc :: IO ()
someFunc = do
  print "List"
  print "filter"
  print $ L.filter ( > 5) [0..10]
  print "length"
  print $ L.length [0..10]
  print "delete 2"
  print $ L.delete [0..10] 2
  print $ L.delete' [0..10] 2
  print $ L.update [0..10] 2 100
  print $ L.update' [0..10] 2 100

