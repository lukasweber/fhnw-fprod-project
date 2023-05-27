module Main where

import qualified MyLib (main)

main :: IO ()
main = do
  MyLib.main
