{-# LANGUAGE OverloadedStrings #-}

module Main where


import System.Environment
import System.IO
import System.Console.GetOpt
import System.Exit

import Control.Monad

import Data.List (filter, intercalate)
import Data.Char (toLower)
import Data.Functor.Identity (runIdentity)

import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import qualified Data.Text as Strict
import qualified Data.Text.IO as Strict

import Text.Pandoc
import Text.Pandoc.Parsing

import qualified Grammar.NBNF as NBNF

import Import.Token
import Import.Parse

import Export.XML
import Export.TXT
import Export.LOG
import Export.HTML



data Flag =
    InFile String   -- -i
  | OutFile String  -- -o
  | Help            -- --help
  | HtmlMode String -- -h
  deriving Eq

isInFile :: Flag -> Bool
isInFile (InFile _) = True
isInFile _ = False

isOutFile :: Flag -> Bool
isOutFile (OutFile _) = True
isOutFile _ = False

isHtmlMode :: Flag -> Bool
isHtmlMode (HtmlMode _) = True
isHtmlMode _ = False

getInFilePaths :: [Flag] -> [String]
getInFilePaths [] = []
getInFilePaths (flag : rest) = case flag of
  InFile path -> path : getInFilePaths rest
  _ -> getInFilePaths rest

getOutFilePaths :: [Flag] -> [String]
getOutFilePaths [] = []
getOutFilePaths (flag : rest) = case flag of
  OutFile path -> path : getInFilePaths rest
  _ -> getInFilePaths rest

options = [
    Option ['i'] ["in"] (ReqArg InFile "FILE")
      "Input file"
  , Option ['o'] ["out"] (ReqArg OutFile "FILE")
      "Output file. Supported file extensions: txt, html, xml, log"
  , Option [] ["help"] (NoArg Help)
      "Print this help message"
  , Option ['h'] [] (ReqArg HtmlMode "HTML-MODE")
      ("In case the format of the output file is HTML you can choose between" ++
      "the following modes: standalone, jekyll")
  ]

data FileFormat = TXT | XML | HTML | LOG

--
getOutFileExtension :: FilePath -> String
getOutFileExtension = reverse . takeWhile (/= '.') . reverse

-- Convert all file extensions in a given list to `FileFormat`
getOutFormat :: String -> IO FileFormat
getOutFormat ext = case ext of
  "txt" -> return TXT
  "xml" -> return XML
  "html" -> return HTML
  "log" -> return LOG
  _ -> die $ "Invalid file format: " ++ ext ++
    "\nChoose one of the following: txt, xml, html"

data HtmlMode =
    StandAlone
  | Jekyll
  deriving Eq

stringToHtmlMode :: String -> IO HtmlMode
stringToHtmlMode "standalone" = return StandAlone
stringToHtmlMode "jekyll" = return Jekyll
stringToHtmlMode str = die $ "Invalid Html mode: " ++ str ++
  "\nChoose one of the following: standalone, jekyll"

-- The path of the HTML template
htmlTemplatePath :: HtmlMode -> FilePath
htmlTemplatePath StandAlone = "html/template_standalone.html"
htmlTemplatePath Jekyll = "html/template_jekyll.html"

-- Intended to be applied only to tokens of type `NbnfCode`
parseNbnfToken' :: Token -> NBNF.Rule
parseNbnfToken' token = case parseNbnfToken token of
  Left err -> error $ "While parsing the NBNF code in lines "
      ++ show (pos token) ++ ", namely\n\n" ++ show (content token) ++ "\n\n"
      ++ "the following error occured:\n\n" ++ show err
  Right nbnfRule -> nbnfRule

-- Convert a token to Markdown text (in case the token is an NBNF code token
-- it is converted to HTML)
tokenToMarkdown :: Token -> Strict.Text
tokenToMarkdown token@(Token tokType pos content) = case tokType of
  NbnfCode -> "\n" <> ruleToHtmlText (parseNbnfToken' token) <> "\n"
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

main :: IO ()
main = do
  args <-  getArgs
  progName <- getProgName

  let header = "Usage: stack exec " ++ progName ++ " --"
        ++ " -i INFILE"
        ++ " -o OUTFILE"
        ++ " -h HTML-MODE"
        ++ " [--help]"

  case getOpt Permute options args of
    (flags,_,[]) -> do
      -- Print help message and exit if the "--help" flag is set
      when (Help `elem` flags) $ do
        hPutStrLn stderr (usageInfo header options)
        exitSuccess

      let inFiles = getInFilePaths $ filter isInFile flags

      outPath <- do
        case filter isOutFile flags of
          [OutFile path] -> return path
          [] -> die $ "No output file given.\n" ++ header
          _ -> die $ "Only one output file is allowed.\n" ++ header

      -- Get the output formats from the file extension of the output files
      let outFileExtension = getOutFileExtension outPath
      outFormat <- getOutFormat outFileExtension

      -- Get the contents of the input files, concatenate them and tokenize the
      -- resulting string
      inHandles <- mapM (`openFile` ReadMode) inFiles
      mdContents <- mapM hGetContents inHandles
      let markdown = intercalate "\n\n" mdContents
      let tokens = tokenize markdown

      -- Convert the input file to a text in a new format
      newContent <- case outFormat of
                      TXT -> do
                        let nbnfTokens = filter isNbnfToken tokens
                        let nbnfGrammar = map parseNbnfToken' nbnfTokens
                        return $ grammarToText nbnfGrammar
                      LOG -> do
                        let nbnfTokens = filter isNbnfToken tokens
                        let nbnfGrammar = map parseNbnfToken' nbnfTokens
                        return $ grammarToLog nbnfGrammar
                      XML -> do
                        let nbnfTokens = filter isNbnfToken tokens
                        let nbnfGrammar = map parseNbnfToken' nbnfTokens
                        return $ grammarToXmlText nbnfGrammar
                      HTML -> do
                        -- Get the HTML mode
                        htmlMode <- do
                          case filter isHtmlMode flags of
                            [HtmlMode mode] -> stringToHtmlMode mode
                            [] -> die $ "No HTML mode given.\n" ++ header
                            _ -> die $ "HTML mode can be set at most once.\n" ++ header

                        -- Get HTML template
                        htmlTemplateHandle <- openFile (htmlTemplatePath htmlMode) ReadMode
                        htmlTemplate <- Strict.hGetContents htmlTemplateHandle

                        let markdownParts = map tokenToMarkdown tokens
                        let newMarkdown = Strict.concat markdownParts
                        html <- mdToHtml newMarkdown htmlTemplate
                        hClose htmlTemplateHandle
                        return $ Lazy.fromStrict html

      -- Print the converted input to a file
      outHandle <- openFile outPath WriteMode
      Lazy.hPutStr outHandle newContent

      -- Close handles
      mapM_ hClose inHandles
      hClose outHandle

    (_,_,errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header options)
      exitWith (ExitFailure 1)
