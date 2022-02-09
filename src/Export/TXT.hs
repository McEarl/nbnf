-- |
-- Module      : Export.TXT
-- Copyright   : (c) 2021 Marcel Schütz
-- License     : BSD 2-clause
--
-- Exporting NBNF to plain text


{-# LANGUAGE OverloadedStrings #-}

module Export.TXT (
  grammarToText
) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Char (isPrint, isSpace, ord, toUpper)
import Data.List (genericLength)
import Numeric (showHex)

import qualified Grammar.NBNF as NBNF
import Import.Token
import Import.Parse


-- | Convert an NBNF grammar to an XML text
grammarToText :: NBNF.Grammar -> Text
grammarToText [] = ""
grammarToText [rule] = ruleToText rule
grammarToText (rule : rest) = ruleToText rule <> "\n\n" <> grammarToText rest

ruleToText :: NBNF.Rule -> Text
-- The outermost expression is a (non-degenerate) choice expression
ruleToText (NBNF.NonTerminalRule identifier (NBNF.Choice (expr : rest))) = let
  indentation = Text.replicate (genericLength $ "<" ++ identifier ++ ">") " "
  in "<" <> Text.pack identifier <> ">" <> " = " <> exprToText expr <>
     Text.concat (map ((("\n" <> indentation <> " | ") <>) . exprToText) rest)
-- The outermost expression is something else
ruleToText (NBNF.NonTerminalRule identifier expression) =
  "<" <> Text.pack identifier <> ">" <> " = " <> exprToText expression
ruleToText (NBNF.AlphabetRule _ []) = ""
ruleToText (NBNF.AlphabetRule identifier (charSelection : rest)) = let
  indentation = Text.replicate (genericLength $ "<<" ++ identifier ++ ">>") " "
  in "<<" <> Text.pack identifier <> ">>" <> " = " <>
    charSelectionToText charSelection <>
    Text.concat (map ((("\n" <> indentation <> " | ") <>) . charSelectionToText) rest)

exprToText :: NBNF.Expression -> Text
exprToText (NBNF.Choice []) = ""
exprToText (NBNF.Choice [expr]) = exprToText expr
exprToText (NBNF.Choice (expr : rest)) = parenExprToText expr <>
  Text.concat (map ((" | " <>) . parenExprToText) rest)
exprToText (NBNF.Sequence []) = ""
exprToText (NBNF.Sequence [expr]) = exprToText expr
exprToText (NBNF.Sequence (expr : rest)) = parenExprToText expr <>
  Text.concat (map ((" " <>) . parenExprToText) rest)
exprToText (NBNF.Option expr) = "[ " <> exprToText expr <> " ]"
exprToText (NBNF.Repetition expr) = "{ " <> exprToText expr <> " }"
exprToText (NBNF.Terminal str) = typedStringToText str
exprToText (NBNF.NonTerminal identifier) = "<" <> Text.pack identifier <> ">"
exprToText (NBNF.Alphabet identifier) = "<<" <> Text.pack identifier <> ">>"
exprToText (NBNF.CharException alphabet chars) = "<<" <> Text.pack alphabet <>
  ">>" <> " \\ {" <> charListToText chars <> "}"
exprToText (NBNF.StringException alphabet operator strings) = "<<" <>
  Text.pack alphabet <> ">>" <> operatorToText operator <> " \\ {" <>
  stringListToText strings <> "}"

-- Enclose a choice expression in parentheses. Necessary if such an
-- expession is part of a sequence or another choice expression.
parenExprToText :: NBNF.Expression -> Text
parenExprToText expr = case expr of
  NBNF.Choice exprs -> "( " <> exprToText expr <> " )"
  _ -> exprToText expr

operatorToText :: NBNF.Operator -> Text
operatorToText NBNF.Star = "*"
operatorToText NBNF.Plus = "+"

charListToText :: [NBNF.TypedChar] -> Text
charListToText [] = ""
charListToText [c] = typedCharToText c
charListToText (c : cs) = typedCharToText c <>
  Text.concat (map ((", " <>) . typedCharToText) cs)

stringListToText :: [NBNF.TypedString] -> Text
stringListToText [] = ""
stringListToText [s] = typedStringToText s
stringListToText (s : ss) = typedStringToText s <>
  Text.concat (map ((", " <>) . typedStringToText) ss)

typedCharToText :: NBNF.TypedChar -> Text
typedCharToText (NBNF.TypedChar strType char) = strDelimiter strType <>
  charToText char <> strDelimiter strType

typedStringToText :: NBNF.TypedString -> Text
typedStringToText (NBNF.TypedString strType str) = strDelimiter strType <>
  stringToText str <> strDelimiter strType

charSelectionToText :: NBNF.CharSelection -> Text
charSelectionToText (NBNF.CharChoice c c') = "\"" <> charToText c <> "\"" <>
  " | ... | " <> "\"" <> charToText c' <> "\""
charSelectionToText (NBNF.SingleChar c) = "\"" <> charToText c <> "\""

charToText :: Char -> Text
charToText c = if isPrint c && not (isSpace c) && c `notElem` ['\"', '\'', '\\']
  then Text.singleton c
  else escape c

stringToText :: String -> Text
stringToText = Text.concat . map charToText

escape :: Char -> Text
escape '\"' = "\\\""
escape '\'' = "\\\'"
escape '\\' = "\\\\"
escape c = "\\U+" <> codePoint c <> ";"
  where
    -- Get the Unicode code point of a character and convert it to Text
    codePoint c = Text.pack. map toUpper $
      let number = showHex (ord c) "" in
        case length number of
          1 -> "000" ++ number
          2 -> "00" ++ number
          3 -> "0" ++ number
          _ -> number

strDelimiter :: NBNF.StringType -> Text
strDelimiter strType = case strType of
  NBNF.CaseSensitive -> Text.pack "\""
  NBNF.CaseInsensitive -> Text.pack "\'"
