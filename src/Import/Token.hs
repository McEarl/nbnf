-- |
-- Module      : Import.Token
-- Copyright   : (c) 2021 Marcel Schütz
-- License     : BSD 2-clause
--
-- Tokenizing Markdown documents which contain NBNF code


module Import.Token (
  Token(..),
  TokenType(..),
  SourceRange(..),
  Int,
  newNBNFTestToken,
  isNbnfToken,
  tokenize
) where


import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (Parser)
import Text.Parsec.Pos


data Token = Token {
    tokType :: TokenType,
    pos :: SourceRange,
    content :: String
  }

data TokenType = NbnfCode | Misc

data SourceRange = SourceRange SourcePos SourcePos

emptyRange :: SourceRange
emptyRange = SourceRange (newPos "" 1 1) (newPos "" 1 1)

newNBNFTestToken :: String -> Token
newNBNFTestToken string = Token NbnfCode emptyRange string

instance Show Token where
  show (Token tokType pos content) = show tokType ++ " [" ++ show pos ++ "]: " ++
    content ++ "\n\n"

instance Show TokenType where
  show NbnfCode = "NBNF code"
  show Misc = "Misc"

instance Show SourceRange where
  show (SourceRange startPos endPos) = show startPos ++ " - " ++ show endPos


-- | Check whether a token contains NBNF code
isNbnfToken :: Token -> Bool
isNbnfToken token = case tokType token of
  NbnfCode -> True
  _ -> False

tokenize :: String -> [Token]
tokenize str = case parse mdTokens "" str of
  Left err -> error $ "While tokenizing the document the following error " ++
    "occured:\n\n" ++ show err
  Right result -> result

mdTokens :: Parser [Token]
mdTokens = many $ choice [
    nbnfToken,
    miscToken
  ]

nbnfToken :: Parser Token
nbnfToken = do
  string "```nbnf"
  optional endOfLine
  startPos <- getPosition
  content <- manyTill anyChar (try $ choice [lookAhead (string "\n```"), lookAhead (string "```")])
  endPos <- getPosition
  optional endOfLine
  string "```"
  optional endOfLine
  return $ Token NbnfCode (SourceRange startPos endPos) content

miscToken :: Parser Token
miscToken = do
  startPos <- getPosition
  content <- try (manyTill anyChar (try (lookAhead (string "```nbnf")))) <|> many1 anyChar
  endPos <- getPosition
  optional endOfLine
  return $ Token Misc (SourceRange startPos endPos) content
