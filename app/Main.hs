{-# LANGUAGE OverloadedStrings #-}

module Main where


import System.Environment
import System.IO

import Data.List (filter, intercalate)
import Data.Char (toLower)
import Data.Functor.Identity (runIdentity)

import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict

import Text.Pandoc


import Import.Token
import Import.Parse
import Export.XML
import Export.PlainText
import Export.HTML


main :: IO ()
main = do
  args <-  getArgs
  progName <- getProgName

  if length args < 2
    then let
      usageHeader = "stack " ++ progName ++ " <input file> <output file>"
      in fail $ "Invalid number of arguments. " ++ "Usage:\n\n" ++ usageHeader
        ++ "\n\nSupported file extensions for <output file> are: txt, xml, html"
    else do
      let inFiles = init args
      let outFile = last args
      -- Get the output format from the file extension of the output file
      let outFileExtension = reverse . takeWhile (/= '.') . reverse $ outFile
      let outFormat = case outFileExtension of
                        "txt" -> TXT
                        "xml" -> XML
                        "html" -> HTML
                        _ -> error $ "Invalid file format for your output file: "
                              ++ outFileExtension ++ "\nSupported formats are: \
                              \txt, xml, html"

      -- Get the contents of the input files, concatenate them and tokenize the
      -- resulting string
      inHandles <- mapM (`openFile` ReadMode) inFiles
      mdContents <- mapM hGetContents inHandles
      let markdown = intercalate "\n\n" mdContents
      let tokens = tokenize markdown

      -- Get HTML template
      htmlTemplateHandle <- openFile htmlTemplatePath ReadMode
      htmlTemplate <- Strict.hGetContents htmlTemplateHandle

      -- Convert the input file to a text in a new format
      newContent <- case outFormat of
                      TXT -> do
                        let nbnfTokens = filter isNbnfToken tokens
                        let nbnfGrammar = map parseNbnfToken nbnfTokens
                        return $ grammarToText nbnfGrammar
                      XML -> do
                        let nbnfTokens = filter isNbnfToken tokens
                        let nbnfGrammar = map parseNbnfToken nbnfTokens
                        return $ grammarToXmlText nbnfGrammar
                      HTML -> do
                        let markdownParts = map tokenToMarkdown tokens
                        let newMarkdown = Strict.concat markdownParts
                        html <- mdToHtml newMarkdown htmlTemplate
                        return $ Lazy.fromStrict html

      -- Print the converted input to a file
      outHandle <- openFile outFile WriteMode
      Lazy.hPutStr outHandle newContent

      -- Close handles
      mapM_ hClose inHandles
      hClose outHandle
      hClose htmlTemplateHandle

data FileFormat = TXT | XML | HTML

-- Convert a token to Markdown text (in case the token is an NBNF code token
-- it is converted to HTML)
tokenToMarkdown :: Token -> Strict.Text
tokenToMarkdown token@(Token tokType _ content) = case tokType of
  NbnfCode -> "\n" <> ruleToHtmlText (parseNbnfToken token) <> "\n"
  Misc -> Strict.pack content

-- Convert a Markdown text to HTML and insert it in an HTML template
mdToHtml :: Strict.Text -> Strict.Text -> IO Strict.Text
mdToHtml markdownText htmlTemplate = runIOorExplode $ do
  md <- readMarkdown readerOpts markdownText
  writeHtml5String writerOpts md
    where
      readerOpts = def {
          readerExtensions = pandocExtensions
        }
      writerOpts = def {
          writerNumberSections = True,
          writerTableOfContents = True,
          writerTOCDepth = 2,
          writerTemplate = Just tocTemplate
        }
      tocTemplate = either error id . runIdentity . compileTemplate "" $ htmlTemplate

-- The path of the HTML template
htmlTemplatePath :: FilePath
htmlTemplatePath = "html/template.html"
