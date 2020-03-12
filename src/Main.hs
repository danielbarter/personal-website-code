{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory ( createDirectoryIfMissing
                        , setCurrentDirectory
                        , doesFileExist
                        )

import System.FilePath.Posix ( (-<.>)
                             , takeBaseName
                             )


import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Crypto.Hash.MD5

main :: IO ()
main = do createDirectoryTree
          processHTML "./content/html/home.html" "./site/index.html"



processHTML :: FilePath -> FilePath -> IO ()
processHTML source target =
  do b <- hasFileChanged source
     if (not b) -- file hasn't changed
       then putStrLn (source ++ " unmodified")
       else do hashFile source
               body <- T.readFile source
               template <- T.readFile "./content/html_templates/default.html"
               let title = T.pack $ takeBaseName source
               let template' = T.replace "$title$" title template
               let result = T.replace "$body$" body template'
               T.writeFile target result
               putStrLn (source ++ " updated")



createDirectoryTree :: IO ()
createDirectoryTree = do mkdir "site"
                         setCurrentDirectory "./site"
                         mkdir "gif"
                         mkdir "img"
                         mkdir "js"
                         mkdir "pdf"
                         setCurrentDirectory "../"
  where mkdir = createDirectoryIfMissing False


hashFile :: FilePath -> IO ()
hashFile path = do bs <- B.readFile path
                   let bsHash = hash bs
                   B.writeFile (path -<.> "md5") bsHash

hasFileChanged :: FilePath -> IO Bool
hasFileChanged path = do b <- doesFileExist (path -<.> "md5")
                         if (not b) -- file doesn't exist
                           then return True
                           else do oldHash <- B.readFile (path -<.> "md5")
                                   bs <- B.readFile path
                                   let newHash = hash bs
                                   return (oldHash /= newHash)
