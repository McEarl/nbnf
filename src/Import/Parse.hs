-- |
-- Module      : Import.Parse
-- Copyright   : (c) 2021 Marcel Schütz
-- License     : BSD 2-clause
--
-- Parsing NBNF rules


module Import.Parse (
  parseNbnfToken
) where


import qualified Grammar.NBNF as NBNF
import Import.Token

import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (Parser)
import Data.Char (isSpace, chr)
import Text.XML.HXT.DOM.Util (hexStringToInt)


-- | Parse a token containing an NBNF rule
parseNbnfToken :: Token -> Either ParseError NBNF.Rule
parseNbnfToken token = parse nbnfRule "" (content token)

nbnfRule :: Parser NBNF.Rule
nbnfRule = try alphabetRule <|> nonTerminalRule


-- * Non-terminal rules

nonTerminalRule :: Parser NBNF.Rule
nonTerminalRule = do
  optional spaces
  lhs <- nonTerminalLhs
  optional horizontalSpaces
  ruleSeparator
  optional horizontalSpaces
  rhs <- nonTerminalRhs
  optional spaces
  eof
  return $ NBNF.NonTerminalRule lhs rhs

nonTerminalLhs :: Parser NBNF.Identifier
nonTerminalLhs = do
  string "<"
  identifier <- nbnfIdentifier
  string ">"
  return identifier

nonTerminalRhs :: Parser NBNF.Expression
nonTerminalRhs = try multilineExpression <|> singleLineExpression

multilineExpression :: Parser NBNF.Expression
multilineExpression = do
  expr1 <- nbnfChoice
  optional horizontalSpaces
  lineBreak
  optional horizontalSpaces
  string "|"
  horizontalSpaces
  expr2 <- nonTerminalRhs
  return $ case expr2 of
    NBNF.Choice exprs -> NBNF.Choice (expr1 : exprs)
    _ -> NBNF.Choice [expr1, expr2]

singleLineExpression :: Parser NBNF.Expression
singleLineExpression = nbnfChoice

-- * Alphabet rules

alphabetRule :: Parser NBNF.Rule
alphabetRule = do
  optional spaces
  lhs <- alphabetLhs
  optional horizontalSpaces
  ruleSeparator
  optional horizontalSpaces
  rhs <- alphabetRhs
  optional spaces
  eof
  return $ NBNF.AlphabetRule lhs rhs

alphabetLhs :: Parser NBNF.Identifier
alphabetLhs = do
  string "<<"
  identifier <- nbnfIdentifier
  string ">>"
  return identifier

alphabetRhs :: Parser [NBNF.CharSelection]
alphabetRhs = sepBy1 (try nbnfCharChoice <|> nbnfSingleChar) sep
  where
    sep = do
      spaces
      string "|"
      spaces

nbnfCharChoice :: Parser NBNF.CharSelection
nbnfCharChoice = do
  string "\""
  char1 <- nbnfChar
  string "\""
  optional horizontalSpaces
  string "|"
  optional horizontalSpaces
  string "..."
  optional horizontalSpaces
  string "|"
  optional horizontalSpaces
  string "\""
  char2 <- nbnfChar
  string "\""
  return $ NBNF.CharChoice char1 char2

nbnfSingleChar :: Parser NBNF.CharSelection
nbnfSingleChar = do
  string "\""
  char <- nbnfChar
  string "\""
  return $ NBNF.SingleChar char


-- * Experssions

nbnfChoice :: Parser NBNF.Expression
nbnfChoice = try properChoice <|> singletonChoice

properChoice :: Parser NBNF.Expression
properChoice = do
  expr1 <- nbnfSequence
  optional horizontalSpaces
  string "|"
  optional horizontalSpaces
  expr2 <- nbnfChoice
  return $ case expr2 of
    NBNF.Choice exprs -> NBNF.Choice (expr1 : exprs)
    _ -> NBNF.Choice [expr1, expr2]

singletonChoice :: Parser NBNF.Expression
singletonChoice = nbnfSequence

nbnfSequence :: Parser NBNF.Expression
nbnfSequence = try properSequence <|> singleExpression

properSequence :: Parser NBNF.Expression
properSequence = do
  expr1 <- singleExpression
  optional horizontalSpaces
  expr2 <- nbnfSequence
  return $ case expr2 of
    NBNF.Sequence exprs -> NBNF.Sequence (expr1 : exprs)
    _ -> NBNF.Sequence [expr1, expr2]

singleExpression :: Parser NBNF.Expression
singleExpression = choice [
    try nbnfOption,
    try nbnfRepetition,
    try nbnfGrouping,
    try nbnfTerminal,
    try nbnfStringException,
    try nbnfCharException,
    try nbnfAlphabet,
    try nbnfNonTerminal
  ]

nbnfOption :: Parser NBNF.Expression
nbnfOption = do
  string "["
  optional horizontalSpaces
  expression <- nbnfChoice
  optional horizontalSpaces
  string "]"
  return $ NBNF.Option expression

nbnfRepetition :: Parser NBNF.Expression
nbnfRepetition = do
  string "{"
  optional horizontalSpaces
  expression <- nbnfChoice
  optional horizontalSpaces
  string "}"
  return $ NBNF.Repetition expression

nbnfGrouping :: Parser NBNF.Expression
nbnfGrouping = do
  string "("
  optional horizontalSpaces
  expression <- nbnfChoice
  optional horizontalSpaces
  string ")"
  return expression

nbnfStringException :: Parser NBNF.Expression
nbnfStringException = do
  alphabet <- nbnfAlphabetIdentifier
  operator <- nbnfOperator
  optional horizontalSpaces
  string "\\"
  optional horizontalSpaces
  string "{"
  optional horizontalSpaces
  strings <- nbnfStringList
  optional horizontalSpaces
  string "}"
  return $ NBNF.StringException alphabet operator strings

nbnfCharException :: Parser NBNF.Expression
nbnfCharException = do
  alphabet <- nbnfAlphabetIdentifier
  optional horizontalSpaces
  string "\\"
  optional horizontalSpaces
  string "{"
  optional horizontalSpaces
  chars <- nbnfCharList
  optional horizontalSpaces
  string "}"
  return $ NBNF.CharException alphabet chars

nbnfAlphabet :: Parser NBNF.Expression
nbnfAlphabet = do
  string "<<"
  identifier <- nbnfIdentifier
  string ">>"
  return $ NBNF.Alphabet identifier

nbnfAlphabetIdentifier :: Parser NBNF.Identifier
nbnfAlphabetIdentifier = do
  string "<<"
  identifier <- nbnfIdentifier
  string ">>"
  return identifier

nbnfNonTerminal :: Parser NBNF.Expression
nbnfNonTerminal = do
  string "<"
  identifier <- nbnfIdentifier
  string ">"
  return $ NBNF.NonTerminal identifier

nbnfIdentifier :: Parser NBNF.Identifier
nbnfIdentifier = many1 (letter <|> digit <|> oneOf [' ', '_' , '-'])

nbnfTerminal :: Parser NBNF.Expression
nbnfTerminal = NBNF.Terminal <$> nbnfTypedString

nbnfOperator :: Parser NBNF.Operator
nbnfOperator = do
  operator <- char '*' <|> char '+'
  return $ case operator of
    '*' -> NBNF.Star
    '+' -> NBNF.Plus
    _ -> undefined

nbnfStringList :: Parser [NBNF.TypedString]
nbnfStringList = sepBy nbnfTypedString listSeparator

nbnfCharList :: Parser [NBNF.TypedChar]
nbnfCharList = sepBy nbnfTypedChar listSeparator


-- * Typed strings/characters

nbnfTypedString :: Parser NBNF.TypedString
nbnfTypedString = choice [
    nbnfCaseSensitiveString,
    nbnfCaseInsensitiveString
  ]

nbnfTypedChar :: Parser NBNF.TypedChar
nbnfTypedChar = choice [
    nbnfCaseSensitiveChar,
    nbnfCaseInsensitiveChar
  ]

nbnfCaseSensitiveString :: Parser NBNF.TypedString
nbnfCaseSensitiveString = do
  char '\"'
  string <- many nbnfChar
  char '\"'
  return $ NBNF.TypedString NBNF.CaseSensitive string

nbnfCaseInsensitiveString :: Parser NBNF.TypedString
nbnfCaseInsensitiveString = do
  char '\''
  string <- many nbnfChar
  char '\''
  return $ NBNF.TypedString NBNF.CaseInsensitive string

nbnfCaseSensitiveChar :: Parser NBNF.TypedChar
nbnfCaseSensitiveChar = do
  char '\"'
  c <- nbnfChar
  char '\"'
  return $ NBNF.TypedChar NBNF.CaseSensitive c

nbnfCaseInsensitiveChar :: Parser NBNF.TypedChar
nbnfCaseInsensitiveChar = do
  char '\''
  c <- nbnfChar
  char '\''
  return $ NBNF.TypedChar NBNF.CaseInsensitive c


-- * Characters

nbnfChar :: Parser Char
nbnfChar = choice [
    try nbnfRegularChar,
    try nbnfUnicodeChar,
    try nbnfEscapedChar
  ]

nbnfRegularChar :: Parser Char
nbnfRegularChar = noneOf ['\"', '\'', '\\']

nbnfUnicodeChar :: Parser Char
nbnfUnicodeChar = do
  string "\\U+"
  codePoint <- many1 hexDigit
  char ';'
  return $ chr (hexStringToInt codePoint)

nbnfEscapedChar :: Parser Char
nbnfEscapedChar = do
  char '\\'
  oneOf ['\"', '\'', '\\']


-- * Misc

ruleSeparator :: Parser String
ruleSeparator = choice [
    string "::=",
    string ":=",
    string "=",
    string "<-",
    string "->"
  ]


-- * Some general parsers

listSeparator :: Parser ()
listSeparator = do
  optional horizontalSpaces
  char ','
  optional horizontalSpaces
  return ()

horizontalSpaces :: Parser String
horizontalSpaces = many1 (satisfy (\c -> isSpace c && c /= '\r' && c /= '\n'))

lineBreak :: Parser String
lineBreak = try (string "\r\n") <|> string "\n"
