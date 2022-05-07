{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory ( createDirectoryIfMissing
                        , setCurrentDirectory
                        , doesFileExist
                        , copyFile
                        , listDirectory
                        )

import System.FilePath.Posix ( (-<.>)
                             , takeBaseName
                             , isExtensionOf
                             )

import Text.Pandoc.Class ( runPure
                         )

import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.LaTeX
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
     processText ( Right . id ) "./content/html/home.html" "./site/index.html"
     processText ( Right . id ) "./content/html/noise.html" "./site/noise.html"
     pmd "./content/markdown/CV.md" "./site/CV.html"
     cvTex "./content/markdown/CV.md" "./content/tex/CV.tex"
     pmd "./content/markdown/publickey.md" "./site/publickey.html"
     pmd "./content/markdown/mix.md" "./site/mix.html"
     pmd "./content/markdown/saddles.md" "./site/saddles.html"
     pmd "./content/markdown/tableau.md" "./site/tableau.html"
     copy "./content/js/mix.js" "./site/js/mix.js"
     copy "./content/js/tableau.js" "./site/js/tableau.js"
     copy "./content/css/style.css" "./site/style.css"
     gifContents <- listDirectory "./content/gif"
     let gifNames = filter (not . isExtensionOf "md5") gifContents
     let gifSources = ("./content/gif/" ++ ) <$> gifNames
     let gifTargets = ("./site/gif/" ++ ) <$> gifNames
     sequence $ zipWith copy gifSources gifTargets
     imgContents <- listDirectory "./content/img"
     let imgNames = filter (not . isExtensionOf "md5") imgContents
     let imgSources = ("./content/img/" ++ ) <$> imgNames
     let imgTargets = ("./site/img/" ++ ) <$> imgNames
     sequence $ zipWith copy imgSources imgTargets
     pdfContents <- listDirectory "./content/pdf"
     let pdfNames = filter (not . isExtensionOf "md5") pdfContents
     let pdfSources = ("./content/pdf/" ++ ) <$> pdfNames
     let pdfTargets = ("./site/pdf/" ++ ) <$> pdfNames
     sequence $ zipWith copy pdfSources pdfTargets
     return ()
  where pmd = processText markdownToHTML


myMarkdownExtensions = foldr enableExtension githubMarkdownExtensions
  [ Ext_backtick_code_blocks
  , Ext_tex_math_dollars
  ]

markdownToHTML :: T.Text -> Either PandocError T.Text
markdownToHTML t =
  runPure $ readMarkdown mdReaderOptions t >>= ( writeHtml5String htmlWriterOptions )
  where mdReaderOptions = def { readerExtensions = myMarkdownExtensions }
        -- there is a defaultMathJax variable in newer versions of pandoc
        htmlWriterOptions = def { writerHTMLMathMethod = MathJax "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js"} 


markdownToTex :: T.Text -> Either PandocError T.Text
markdownToTex t =
  runPure $ readMarkdown mdReaderOptions t >>= ( writeLaTeX def )
  where mdReaderOptions = def { readerExtensions = myMarkdownExtensions }

-- TODO: factor out hash checking
copy :: FilePath -> FilePath -> IO ()
copy source destination =
  do b <- hasFileChanged source
     if (not b)
       then return ()
       else do hashFile source
               copyFile source destination
               putStrLn ("copied " ++ source)

cvTex :: FilePath -> FilePath -> IO ()
cvTex source destination =
  do body <- T.readFile source
     case markdownToTex body of
       Left err -> putStrLn $ show err
       Right result -> do
         T.writeFile destination result
         putStrLn ("produced " ++ destination)

processText :: (T.Text -> Either PandocError T.Text)
            -> FilePath -> FilePath -> IO ()
processText transformer source target =
  do b <- hasFileChanged source
     if (not b) -- file hasn't changed
       then return ()
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
                   putStrLn ("updated " ++ source)



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
