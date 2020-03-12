module Main where

import System.Directory ( createDirectoryIfMissing
                        , setCurrentDirectory
                        )

main :: IO ()
main = createDirectoryTree


createDirectoryTree :: IO ()
createDirectoryTree = do createDirectoryIfMissing False "site"
                         setCurrentDirectory "./site"
                         createDirectoryIfMissing False "gif"
                         createDirectoryIfMissing False "img"
                         createDirectoryIfMissing False "js"
                         createDirectoryIfMissing False "pdf"
                         setCurrentDirectory "../"
