-- |
-- Module      : Import.Token
-- Copyright   : (c) 2021 Marcel Schütz
-- License     : MIT
--
-- Tokenizing Markdown documents which contain NBNF code


module Import.Token (
  Token(..),
  TokenType(..),
  SourceRange(..),
  Int,
  isNbnfToken,
  tokenize
) where


import Text.Parsec
import Text.ParserCombinators.Parsec.Prim (Parser)


data Token = Token {
    tokType :: TokenType,
    pos :: SourceRange,
    content :: String
  }

data TokenType = NbnfCode | Misc

data SourceRange = SourceRange SourcePos SourcePos


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

{-}
-- | Split a Markdown text into tokens, i.e. split it into tokens containing the
--   code of `nbnf` code blocks and tokens containing text between such code
--   blocks
tokenize_old :: String -> [Token]
tokenize_old text = reverse $ split Misc text 1 1 "" []
  where
    split :: TokenType  -- Current token type
          -> String     -- Remaining string to be tokenized
          -> Int       -- Int in which the current lexeme starts
          -> Int       -- Current line number
          -> String     -- Current lexeme (in reversed order!)
          -> [Token]    -- Tokens already generated (in reversed order!)
          -> [Token]
    -- Nothing to tokenize and no lexeme remaining
    split tokType "" startInt currentLine "" tokens = tokens
    -- Nothing to tokenize but a remaining lexeme has to be wrapped in a token
    split tokType "" startInt currentLine currentLexeme tokens =
      newToken tokType startInt currentLine currentLexeme : tokens
    -- The next character(s) mark(s) the beginning of an NBNF code block or
    -- continue(s) the current text between two such code blocks
    split Misc string@(x : xs) startingLine currentLine currentLexeme tokens
      | "\n```nbnf" `isPrefixOf` string =
          split
            NbnfCode
            (takePrefix "\n```nbnf" string)
            (currentLine + 1)
            (currentLine + 1)
            ""
            (newToken Misc startingLine currentLine currentLexeme : tokens)
      | "```nbnf" `isPrefixOf` string =
        split
          NbnfCode
          (takePrefix "```nbnf" string)
          currentLine
          currentLine
          ""
          (newToken Misc startingLine currentLine currentLexeme : tokens)
      | otherwise =
          split
            Misc
            xs
            startingLine
            (if x == '\n' then currentLine + 1 else currentLine)
            (x : currentLexeme)
            tokens
    -- The next character(s) mark(s) the end of an NBNF code block or
    -- continue(s) the current code block
    split NbnfCode string@(x : xs) startingLine currentLine currentLexeme tokens
      | "```\n" `isPrefixOf` string =
          split
            Misc
            (takePrefix "```\n" string)
            (currentLine + 1)
            (currentLine + 1)
            ""
            (newToken NbnfCode startingLine currentLine currentLexeme : tokens)
      | "```" `isPrefixOf` string =
          split
            Misc
            (takePrefix "```" string)
            currentLine
            currentLine
            ""
            (newToken NbnfCode startingLine currentLine currentLexeme : tokens)
      | otherwise =
          split
            NbnfCode
            xs
            startingLine
            (if x == '\n' then currentLine + 1 else currentLine)
            (x : currentLexeme)
            tokens
    -- Take as much characters from a string as the length of a given prefix
    takePrefix prefix str = take (length prefix) str
    -- Create a new token
    newToken tokType startInt endInt lexeme =
      Token tokType (SourceRange startInt endInt) (reverse lexeme)
-}
