module Main where

import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = createDirectoryIfMissing False "site"
