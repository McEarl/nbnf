-- |
-- Module      : Export.LOG
-- Copyright   : (c) 2021 Marcel Schütz
-- License     : BSD 2-clause
--
-- Exporting NBNF to plain text


{-# LANGUAGE OverloadedStrings #-}

module Export.LOG (
  grammarToLog,
  ruleToLog
) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import Data.Char (isPrint, isSpace, ord, toUpper)
import Data.List (genericLength)
import Numeric (showHex)

import qualified Grammar.NBNF as NBNF
import Import.Token
import Import.Parse


-- | Print the internal structure of a given grammar (for debugging)
grammarToLog :: NBNF.Grammar -> Text
grammarToLog [] = ""
grammarToLog [rule] = ruleToText 0 rule
grammarToLog (rule : rest) = ruleToText 0 rule <> "\n" <> grammarToLog rest

-- | Print the internal structure of a given rule (for testing)
ruleToLog :: NBNF.Rule -> String
ruleToLog = Text.unpack . ruleToText 0

-- (The 1st parameter is the initial indentation level)
ruleToText :: Int -> NBNF.Rule -> Text
-- The outermost expression is a (non-degenerate) choice expression
ruleToText n (NBNF.NonTerminalRule identifier (NBNF.Choice (expr : rest))) =
  "NonTerminalRule: [\n" <>
  indent (n + 1) <> "Identifier: \"" <> Text.pack identifier <> "\",\n" <>
  indent (n + 1) <> "Choice: [\n" <>
  exprToText (n + 2) expr <> Text.concat (map ((",\n" <>) . exprToText (n + 2)) rest) <> "\n" <>
  indent (n + 1) <> "]\n" <>
  indent n <> "]"
-- The outermost expression is something else
ruleToText n (NBNF.NonTerminalRule identifier expression) =
  "NonTerminalRule: [\n" <>
  indent (n + 1) <> "Identifier: \"" <> Text.pack identifier <> "\",\n" <>
  exprToText (n + 1) expression <> "\n" <>
  indent n <> "]"
ruleToText n (NBNF.AlphabetRule identifier []) =
  "AlphabetRule: [\n" <>
  indent (n + 1) <> "Identifier: \"" <> Text.pack identifier <> "\",\n" <>
  indent (n + 1) <> "CharacterSelection: []\n" <>
  indent n <> "]"
ruleToText n (NBNF.AlphabetRule identifier (charSelection : rest)) =
  "AlphabetRule: [\n" <>
  indent (n + 1) <> "Identifier: \"" <> Text.pack identifier <> "\",\n" <>
  indent (n + 1) <> "CharacterSelection: [\n" <> charSelectionToText (n + 2) charSelection <> Text.concat (map ((",\n" <>) . charSelectionToText (n + 2)) rest) <> "\n" <>
  indent (n + 1) <> "]\n" <>
  indent n <> "]"

exprToText :: Int -> NBNF.Expression -> Text
exprToText n (NBNF.Choice []) = indent n <> "Choice: []"
exprToText n (NBNF.Choice [expr]) = indent n <> "Choice: [\n" <>
  exprToText (n + 1) expr <> "\n" <>
  indent n <> "]"
exprToText n (NBNF.Choice (expr : rest)) = indent n <> "Choice: [\n" <>
  exprToText (n + 1) expr <> Text.concat (map ((",\n" <>) . exprToText (n + 1)) rest) <> "\n" <>
  indent n <> "]"
exprToText n (NBNF.Sequence []) = indent n <> "Sequence: []"
exprToText n (NBNF.Sequence [expr]) = indent n <> "Sequence: [\n" <>
  exprToText (n + 1) expr <> "\n" <>
  indent n <> "]"
exprToText n (NBNF.Sequence (expr : rest)) = indent n <> "Sequence: [\n" <>
  exprToText (n + 1) expr <> Text.concat (map ((",\n" <>) . exprToText (n + 1)) rest) <> "\n" <>
  indent n <> "]"
exprToText n (NBNF.Option expr) = indent n <> "Option: [\n" <>
  exprToText (n + 1) expr <> "\n" <>
  indent n <> "]"
exprToText n (NBNF.Repetition expr) = indent n <> "Repetition: [\n" <>
  exprToText (n + 1) expr <> "\n" <>
  indent n <> "]"
exprToText n (NBNF.Terminal str) = indent n <> "Terminal: [\n" <>
  typedStringToText (n + 1) str <> "\n" <>
  indent n <> "]"
exprToText n (NBNF.NonTerminal identifier) = indent n <> "NonTerminal: \"" <> Text.pack identifier <> "\""
exprToText n (NBNF.Alphabet identifier) = indent n <> "Alphabet: \"" <> Text.pack identifier <> "\""
exprToText n (NBNF.CharException alphabet chars) = indent n <> "CharException: [\n" <>
  indent (n + 1) <> "Alphabet: \n" <> indent (n + 2) <> Text.pack alphabet <> ",\n" <>
  charListToText (n + 1) chars <> "\n" <>
  indent n <> "]"
exprToText n (NBNF.StringException alphabet operator strings) = indent n <> "StringException: [\n" <>
  indent (n + 1) <> "Alphabet: \n" <> indent (n + 2) <> Text.pack alphabet <> ",\n" <>
  operatorToText (n + 1) operator <> ",\n" <>
  stringListToText (n + 1) strings <> "\n" <>
  indent n <> "]"

operatorToText :: Int -> NBNF.Operator -> Text
operatorToText n NBNF.Star = indent n <> "Operator: *"
operatorToText n NBNF.Plus = indent n <> "Operator: +"

charListToText :: Int -> [NBNF.TypedChar] -> Text
charListToText n [] = indent n <> "CharList: []"
charListToText n [c] = indent n <> "CharList: [\n" <>
  typedCharToText (n + 1) c <> "\n" <>
  indent n <> "]"
charListToText n (c : cs) = indent n <> "CharList: [\n" <>
  typedCharToText (n + 1) c <> Text.concat (map ((",\n" <>) . typedCharToText (n + 1)) cs) <> "\n" <>
  indent n <> "]"

stringListToText :: Int -> [NBNF.TypedString] -> Text
stringListToText n [] = indent n <> "StringList: []"
stringListToText n [s] = indent n <> "StringList: [\n" <>
  typedStringToText (n + 1) s <> "\n" <>
  indent n <> "]"
stringListToText n (s : ss) = indent n <> "StringList: [\n" <>
  typedStringToText (n + 1) s <> Text.concat (map ((",\n" <>) . typedStringToText (n + 1)) ss) <> "\n" <>
  indent n <> "]"

typedCharToText :: Int -> NBNF.TypedChar -> Text
typedCharToText n (NBNF.TypedChar strType char) = indent n <> "TypedChar: [\n" <>
  indent (n + 1) <> strTypeToText strType <> ",\n" <>
  indent (n + 1) <> "Char: \"" <> charToText char <> "\"\n" <>
  indent n <> "]"

typedStringToText :: Int -> NBNF.TypedString -> Text
typedStringToText n (NBNF.TypedString strType str) = indent n <> "TypedString: [\n" <>
  indent (n + 1) <> strTypeToText strType <> ",\n" <>
  indent (n + 1) <> "String: \"" <> stringToText str <> "\"\n" <>
  indent n <> "]"

charSelectionToText :: Int -> NBNF.CharSelection -> Text
charSelectionToText n (NBNF.CharChoice c c') = indent n <> "CharSelection: [\n" <>
  indent (n + 1) <> "CharChoice: [\n" <>
  indent (n + 2) <> "From: \"" <> charToText c <> "\",\n" <>
  indent (n + 2) <> "To: \"" <> charToText c' <> "\"\n" <>
  indent (n + 1) <> "]\n" <>
  indent n <> "]"
charSelectionToText n (NBNF.SingleChar c) = indent n <> "CharSelection: [\n" <>
  indent (n + 1) <> "SingleChar: \"" <> charToText c <> "\"\n" <>
  indent n <> "]"

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

strTypeToText :: NBNF.StringType -> Text
strTypeToText strType = case strType of
  NBNF.CaseSensitive -> "StringType: \"CaseSensitive\""
  NBNF.CaseInsensitive -> "StringType: \"CaseInsensitive\""

-- Indent a text by 2n white spaces
indent :: Int -> Text
indent n = Text.replicate (fromIntegral (2 * n)) " "
