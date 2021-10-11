-- |
-- Module      : Export.HTML
-- Copyright   : (c) 2021 Marcel Schütz
-- License     : BSD 2-clause
--
-- Exporting NBNF to HTML


module Export.HTML (
  ruleToHtmlText
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Html
import Data.List (intersperse, intercalate)
import Data.Char (isPrint, isSpace, ord, toUpper)
import Numeric (showHex)

import qualified Grammar.NBNF as NBNF


-- | Convert an NBNF rule to HTML
ruleToHtmlText :: NBNF.Rule -> Text
ruleToHtmlText = Text.pack . showElement . ruleToElement
  where
    showElement (HtmlTag tag attributes content) =
      "<" ++ tag ++ " " ++ showAttributes attributes ++ ">" ++
      showContent content ++ "</" ++ tag ++ ">"
    showElement (HtmlString string) = string
    showAttributes attributes = intercalate " " (map showAttribute attributes)
    showAttribute (HtmlAttr key val) = key ++ "=\"" ++ val ++ "\""
    showContent (Html elements) = concatMap showElement elements

noAttributes :: [HtmlAttr]
noAttributes = []

-- Take a list of HTML elements and wrap them in a table row
simpleTr :: [HtmlElement] -> HtmlElement
simpleTr elements = HtmlTag "tr" noAttributes (Html (map simpleTd elements))
  where
    simpleTd element = HtmlTag "td" noAttributes (Html [element])


-- Convert an NBNF rule to an HTML table, i.e. to something like this:
--
-- <table class="NBNF_nonterm_rule">
--   <tr>
--     <td><span id="NBNF_NONTERM_identifier" class="NBNF_nonterm_rule_lhs">&#x2329;identifier&#x232A;</span></td>
--     <td>=</td>
--     <td>...</td>
--   </tr>
--   <tr>
--     <td></td>
--     <td>|</td>
--     <td>...</td>
--   </tr>
--   <tr>
--     <td></td>
--     <td>|</td>
--     <td>...</td>
--   </tr>
-- </table>
ruleToElement :: NBNF.Rule -> HtmlElement

-- Outer expression is an empty choice
ruleToElement (NBNF.NonTerminalRule identifier (NBNF.Choice [])) =
  HtmlTag tag attributes content
    where
      tag = "table"
      attributes = [nonTerminalClass]
      content = Html [simpleTr [
          nonTerminalRuleLhsToElement identifier,
          HtmlString "=",
          HtmlString ""
        ]]

-- Outer expression is a proper choice
ruleToElement (NBNF.NonTerminalRule identifier (NBNF.Choice (expression : rest))) =
  HtmlTag tag attributes content
    where
      tag = "table"
      attributes = [nonTerminalClass]
      content = Html $ simpleTr [
          nonTerminalRuleLhsToElement identifier,
          HtmlString "=",
          exprToElement expression
        ] : map additionalRow rest

      additionalRow expr = simpleTr [
          HtmlString "",
          HtmlString bar,
          exprToElement expr
        ]

-- Outer expression is something else
ruleToElement (NBNF.NonTerminalRule identifier expression) =
  HtmlTag tag attributes content
    where
      tag = "table"
      attributes = [nonTerminalClass]
      content = Html [simpleTr [
          nonTerminalRuleLhsToElement identifier,
          HtmlString "=",
          exprToElement expression
        ]]

-- Empty character selection
ruleToElement (NBNF.AlphabetRule identifier []) =
  HtmlTag tag attributes content
    where
      tag = "table"
      attributes = [alphabetClass]
      content = Html [simpleTr [
          alphabetRuleLhsToElement identifier,
          HtmlString "=",
          HtmlString ""
        ]]

-- Non-empty character selection
ruleToElement (NBNF.AlphabetRule identifier (char : rest)) =
  HtmlTag tag attributes content
    where
      tag = "table"
      attributes = [alphabetClass]
      content = Html $ simpleTr [
          alphabetRuleLhsToElement identifier,
          HtmlString "=",
          charSelectionToElement char
        ] : map additionalRow rest

      additionalRow c = simpleTr [
          HtmlString "",
          HtmlString bar,
          charSelectionToElement c
        ]

nonTerminalRuleLhsToElement :: NBNF.Identifier -> HtmlElement
nonTerminalRuleLhsToElement identifier =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [
          HtmlAttr "id" (nonTerminalId identifier),
          HtmlAttr "class" "NBNF_nonterm_rule_lhs"
        ]
      content = Html [HtmlString (leftAngle ++ identifier ++ rightAngle)]

alphabetRuleLhsToElement :: NBNF.Identifier -> HtmlElement
alphabetRuleLhsToElement identifier =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [
          HtmlAttr "id" (alphabetId identifier),
          HtmlAttr "class" "NBNF_alphabet_rule_lhs"
        ]
      content = Html [HtmlString (doubleLeftAngle ++ identifier ++ doubleRightAngle)]

-- Replace all spaces by underscores in a given string
replaceSpaces :: String -> String
replaceSpaces "" = ""
replaceSpaces (c : cs) = let c' = if c == ' ' then '_' else c in
  c' : replaceSpaces cs


exprToElement :: NBNF.Expression -> HtmlElement

-- Choice
exprToElement (NBNF.Choice []) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_choice"]
    content = Html [HtmlString ""]
exprToElement (NBNF.Choice [expr]) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_choice"]
    content = Html [exprToElement expr]
exprToElement (NBNF.Choice expressions) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_choice"]
    content = Html $ intersperse (HtmlString (" " ++ bar ++ " ")) (map exprToElement expressions)

-- Sequence
exprToElement (NBNF.Sequence []) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_sequence"]
    content = Html [HtmlString ""]
exprToElement (NBNF.Sequence [expr]) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_sequence"]
    content = Html [exprToElement expr]
exprToElement (NBNF.Sequence expressions) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_sequence"]
    content = Html $ intersperse (HtmlString " ") (concatMap parenExprToElements expressions)

-- Option
exprToElement (NBNF.Option expression) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_option"]
    content = Html $ [HtmlString "[ "] ++ [exprToElement expression] ++ [HtmlString " ]"]

-- Repetition
exprToElement (NBNF.Repetition expression) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_repetition"]
    content = Html $ [HtmlString "{ "] ++ [exprToElement expression] ++ [HtmlString " }"]

-- Terminal
exprToElement (NBNF.Terminal str) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_terminal"]
    content = Html [typedStringToElement str]

-- Non-terminal
exprToElement (NBNF.NonTerminal identifier) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_non_terminal"]
    content = Html [HtmlTag "a" [HtmlAttr "href" ("#" ++ nonTerminalId identifier)]
      (Html [HtmlString (leftAngle ++ identifier ++ rightAngle)])]

-- Alphabet
exprToElement (NBNF.Alphabet identifier) = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_alphabet"]
    content = Html [HtmlTag "a" [HtmlAttr "href" ("#" ++ alphabetId identifier)]
      (Html [HtmlString (doubleLeftAngle ++ identifier ++ doubleRightAngle)])]

-- Character exception
exprToElement (NBNF.CharException identifier chars) =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [HtmlAttr "class" "NBNF_char_exception"]
      content = Html [exprToElement (NBNF.Alphabet identifier),
          HtmlString ( " " ++ backslash ++ " "),
          HtmlString "{",
          charListToElement chars,
          HtmlString "}"
        ]

-- String exception
exprToElement (NBNF.StringException identifier operator strings) =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [HtmlAttr "class" "NBNF_string_exception"]
      content = Html [exprToElement (NBNF.Alphabet identifier),
          operatorToElement operator,
          HtmlString (" " ++ backslash ++ " "),
          HtmlString "{",
          stringListToElement strings,
          HtmlString "}"
        ]


charListToElement :: [NBNF.TypedChar] -> HtmlElement
charListToElement chars = HtmlTag  tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_char_list"]
    content = Html $ intersperse (HtmlString ", ") (map typedCharToElement chars)

stringListToElement :: [NBNF.TypedString] -> HtmlElement
stringListToElement strings = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_string_list"]
    content = Html $ intersperse (HtmlString ", ") (map typedStringToElement strings)

typedCharToElement :: NBNF.TypedChar -> HtmlElement
typedCharToElement (NBNF.TypedChar NBNF.CaseSensitive char) =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [HtmlAttr "class" "NBNF_case_sensitive_char"]
      content = Html [
          HtmlString "\"",
          escapeChar char,
          HtmlString "\""
        ]
typedCharToElement (NBNF.TypedChar NBNF.CaseInsensitive char) =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [HtmlAttr "class" "NBNF_case_insensitive_char"]
      content = Html [
          HtmlString "\'",
          escapeChar char,
          HtmlString "\'"
        ]

typedStringToElement :: NBNF.TypedString -> HtmlElement
typedStringToElement (NBNF.TypedString NBNF.CaseSensitive string) =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [HtmlAttr "class" "NBNF_case_sensitive_string"]
      content = Html (
          [HtmlString "\""] ++
          map escapeChar string ++
          [HtmlString "\""]
        )
typedStringToElement (NBNF.TypedString NBNF.CaseInsensitive string) =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [HtmlAttr "class" "NBNF_case_insensitive_string"]
      content = Html (
          [HtmlString "\'"] ++
          map escapeChar string ++
          [HtmlString "\'"]
        )

operatorToElement :: NBNF.Operator -> HtmlElement
operatorToElement operator = HtmlTag tag attributes content
  where
    tag = "span"
    attributes = [HtmlAttr "class" "NBNF_operator"]
    content = case operator of
      NBNF.Star -> Html [HtmlString "*"]
      NBNF.Plus -> Html [HtmlString "+"]

-- Enclose a choice expression in parentheses
parenExprToElements :: NBNF.Expression -> [HtmlElement]
parenExprToElements expr@(NBNF.Choice expressions) = [
    HtmlString "( ",
    exprToElement expr,
    HtmlString " )"
  ]
parenExprToElements expr = [exprToElement expr]

escapeChar :: Char -> HtmlElement
escapeChar c = if isPrint c && not (isSpace c) && c `notElem` ['\"', '\'', '\\']
  then HtmlString (htmlEscape c)
  else HtmlTag "span" [HtmlAttr "class" "NBNF_escaped"] (Html [HtmlString (escape c)])
  where
    escape '\"' = backslash ++ "\""
    escape '\'' = backslash ++  "\'"
    escape '\\' = backslash ++ backslash
    escape c = backslash ++ "U+" <> codePoint c <> ";"
    codePoint c = map toUpper $
      let number = showHex (ord c) "" in
        case length number of
          1 -> "000" ++ number
          2 -> "00" ++ number
          3 -> "0" ++ number
          _ -> number
    -- To avoid problems with pandoc the following symbols must be escaped
    htmlEscape '[' = leftBracket
    htmlEscape ']' = rightBracket
    htmlEscape '\\' = backslash
    htmlEscape '|' = bar
    htmlEscape c = [c]

charSelectionToElement :: NBNF.CharSelection -> HtmlElement
charSelectionToElement (NBNF.CharChoice c c') =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [HtmlAttr "class" "NBNF_char_choice"]
      content = Html [
          charToElement c,
          HtmlString (" " ++ bar ++ " ... " ++ bar ++ " "),
          charToElement c'
        ]
charSelectionToElement (NBNF.SingleChar c) =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [HtmlAttr "class" "NBNF_single_char"]
      content = Html [charToElement c]

charToElement :: Char -> HtmlElement
charToElement char =
  HtmlTag tag attributes content
    where
      tag = "span"
      attributes = [HtmlAttr "class" "NBNF_char"]
      content = Html [
          HtmlString "\"",
          escapeChar char,
          HtmlString "\""
        ]


nonTerminalClass :: HtmlAttr
nonTerminalClass = HtmlAttr "class" "NBNF_nonterm_rule"

alphabetClass :: HtmlAttr
alphabetClass = HtmlAttr "class" "NBNF_alphabet_rule"

alphabetId :: NBNF.Identifier -> String
alphabetId identifier = "NBNF_ALPHABET_" ++ replaceSpaces identifier

nonTerminalId :: NBNF.Identifier -> String
nonTerminalId identifier = "NBNF_NONTERMINAL_" ++ replaceSpaces identifier

bar :: String
bar = "&#x007C;"

backslash :: String
backslash = "&#x005C;"

leftAngle :: String
leftAngle = "&#x27E8;"

rightAngle :: String
rightAngle = "&#x27E9;"

doubleLeftAngle :: String
doubleLeftAngle = "&#x27EA;"

doubleRightAngle :: String
doubleRightAngle = "&#x27EB;"

leftBracket :: String
leftBracket = "&#x005B;"

rightBracket :: String
rightBracket = "&#x005BD;"
