{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory ( createDirectoryIfMissing
                        , setCurrentDirectory
                        , doesFileExist
                        )

import System.FilePath.Posix ( (-<.>)
                             , takeBaseName
                             )

import Text.Pandoc.Class ( runPure
                         )

import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options
import Text.Pandoc.Error
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Crypto.Hash.MD5


main :: IO ()
main =
  do setCurrentDirectory "/home/danielbarter/personal-website-code"
     createDirectoryTree
     process ( Right . id ) "./content/html/home.html" "./site/index.html"
     process markdownToHTML "./content/markdown/CV.md" "./site/CV.html"
     process markdownToHTML "./content/markdown/publickey.md" "./site/publickey.html"
     process markdownToHTML "./content/markdown/mix.md" "./site/mix.html"
     process markdownToHTML "./content/markdown/tableau.md" "./site/tableau.html"



markdownToHTML :: T.Text -> Either PandocError T.Text
markdownToHTML t = runPure $ readMarkdown def t >>= ( writeHtml5String def )

process :: (T.Text -> Either PandocError T.Text)
        -> FilePath -> FilePath -> IO ()
process transformer source target =
  do b <- hasFileChanged source
     if (not b) -- file hasn't changed
       then putStrLn (source ++ " unmodified")
       else do hashFile source
               body <- T.readFile source
               case transformer body of
                 Left err -> putStrLn $ show err
                 Right body' -> do
                   template <- T.readFile "./content/html_templates/default.html"
                   let title = T.pack $ takeBaseName source
                   let template' = T.replace "$title$" title template
                   let result = T.replace "$body$" body' template'
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
