-- |
-- Module      : Export.XML
-- Copyright   : (c) 2021 Marcel Schütz
-- License     : MIT
--
-- Exporting NBNF to XML


{-# LANGUAGE OverloadedStrings #-}

module Export.XML (
  grammarToXmlText
) where

import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as Text
import qualified Text.XML as XML
import qualified Data.Map.Strict as Map

import qualified Grammar.NBNF as NBNF
import Import.Token
import Import.Parse


-- * Helpers

type Attributes = Map.Map XML.Name Text

-- Convert a key/value list to XML attributes
attributes :: [(Text, Text)] -> Attributes
attributes = Map.fromList . keysToName
  where
    -- Convert all keys of a key/value list to XML names
    keysToName [] = []
    keysToName ((key, value) : rest) = (keyToName key, value) : keysToName rest
    -- Convert a single key to an XML name
    keyToName text = XML.Name text Nothing Nothing

noAttributes :: Attributes
noAttributes = Map.empty

-- Create an XML node element out of a name, attributes and a list of nodes
nodeElement :: Text -> Attributes -> [XML.Node] -> XML.Node
nodeElement name attributes nodes = XML.NodeElement $ XML.Element elementName attributes nodes
  where
    elementName = XML.Name name Nothing Nothing


-- * Convert NBNF to XML

-- Convert an NBNF grammar to an XML element
grammarToElement :: NBNF.Grammar -> XML.Element
grammarToElement grammar = XML.Element "nbnf" noAttributes (map ruleToNode grammar)

-- Convert an NBNF rule to an XML node
ruleToNode :: NBNF.Rule -> XML.Node
ruleToNode (NBNF.NonTerminalRule identifier expression) =
  nodeElement "non_terminal_rule" noAttributes [lhs, rhs]
  where
    lhs = nodeElement "identifier" noAttributes [XML.NodeContent $ Text.pack identifier]
    rhs = nodeElement "expression" noAttributes [expressionToNode expression]
ruleToNode (NBNF.AlphabetRule identifier charLists) =
  nodeElement "alphabet_rule" noAttributes [lhs, rhs]
  where
    lhs = nodeElement "identifier" noAttributes [XML.NodeContent $ Text.pack identifier]
    rhs = nodeElement "characters" noAttributes (map charSelectionToNode charLists)

-- Convert an NBNF expression to an XML node
expressionToNode :: NBNF.Expression -> XML.Node
expressionToNode (NBNF.Choice expressions) =
  nodeElement "choice" noAttributes (map expressionToNode expressions)
expressionToNode (NBNF.Sequence expressions) =
  nodeElement "sequence" noAttributes (map expressionToNode expressions)
expressionToNode (NBNF.Option expression) =
  nodeElement "option" noAttributes [expressionToNode expression]
expressionToNode (NBNF.Repetition expression) =
  nodeElement "repetition" noAttributes [expressionToNode expression]
expressionToNode (NBNF.Terminal string) =
  nodeElement "terminal" noAttributes [typedStringToNode string]
expressionToNode (NBNF.NonTerminal identifier) =
  nodeElement "non_terminal" noAttributes [identifierToNode identifier]
expressionToNode (NBNF.Alphabet identifier) =
  nodeElement "alphabet" noAttributes [identifierToNode identifier]
expressionToNode (NBNF.CharException identifier chars) =
  nodeElement "character_exception" noAttributes [
      nodeElement "alphabet" noAttributes [identifierToNode identifier],
      nodeElement "characters" noAttributes (map typedCharToNode chars)
    ]
expressionToNode (NBNF.StringException identifier operator strings) =
  nodeElement "string_exception" (attributes [("operator", print operator)]) [
      nodeElement "alphabet" noAttributes [identifierToNode identifier],
      nodeElement "strings" noAttributes (map typedStringToNode strings)
    ]
  where
    print NBNF.Star = "star"
    print NBNF.Plus = "plus"

-- Convert a typed string to an XML node
typedStringToNode :: NBNF.TypedString -> XML.Node
typedStringToNode (NBNF.TypedString stringType string) = let
  value = case stringType of
    NBNF.CaseSensitive -> "case_sensitive"
    NBNF.CaseInsensitive -> "case_insensitive"
  in nodeElement "string" (attributes [("type", value)]) [XML.NodeContent $ Text.pack string]

-- Convert a typed character to an XML node
typedCharToNode :: NBNF.TypedChar -> XML.Node
typedCharToNode (NBNF.TypedChar stringType char) = let
  value = case stringType of
    NBNF.CaseSensitive -> "case_sensitive"
    NBNF.CaseInsensitive -> "case_insensitive"
  in nodeElement "character" (attributes [("type", value)]) [XML.NodeContent $ Text.singleton char]

-- Convert an identifier to an XML node
identifierToNode :: NBNF.Identifier -> XML.Node
identifierToNode identifier = XML.NodeContent (Text.pack identifier)

-- Convert a character list to an XML node
charSelectionToNode :: NBNF.CharSelection -> XML.Node
charSelectionToNode (NBNF.CharChoice c c') =
  nodeElement "char_list" noAttributes [node, node']
  where
    node  = nodeElement "fst_char" noAttributes [XML.NodeContent $ Text.singleton c ]
    node' = nodeElement "snd_char" noAttributes [XML.NodeContent $ Text.singleton c']
charSelectionToNode (NBNF.SingleChar c) =
    nodeElement "single_char" noAttributes [XML.NodeContent $ Text.singleton c]


-- * Rendering

-- Convert a grammar to an XML document
grammarToXmlDoc :: NBNF.Grammar -> XML.Document
grammarToXmlDoc grammar = XML.Document xmlPrologue xmlRoot xmlEpilogue
  where
    xmlPrologue = XML.Prologue [] Nothing []
    xmlRoot = grammarToElement grammar
    xmlEpilogue = []

-- | Convert an NBNF grammar to an XML text
grammarToXmlText :: NBNF.Grammar -> LazyText.Text
grammarToXmlText grammar = XML.renderText renderSettings (grammarToXmlDoc grammar)

renderSettings :: XML.RenderSettings
renderSettings = XML.def {XML.rsPretty = True}
