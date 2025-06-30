module Main where

import qualified Larousse

main :: IO ()
main = do
  defs <- Larousse.lookup "chat"
  print defs
