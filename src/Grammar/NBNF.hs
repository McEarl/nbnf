-- |
-- Module      : Grammar.NBNF
-- Copyright   : (c) 2021 Marcel Schütz
-- License     : BSD 2-clause
--
-- A variation of BNF called NBNF (eNriched BNF) which provides some syntactic
-- sugar for BNF


module Grammar.NBNF (
  Grammar(..),
  Rule(..),
  Identifier,
  Expression(..),
  TypedChar(..),
  TypedString(..),
  StringType(..),
  Operator(..),
  CharSelection(..)
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


type Grammar = [Rule]

-- | An NBNF rule: Either the definition of a non-terminal or of an alphabet
data Rule =
    NonTerminalRule {
        lhs :: Identifier,
        expr :: Expression
      }
  | AlphabetRule {
        lhs :: Identifier,
        chars :: [CharSelection]
      }
  deriving (Eq)

type Identifier = String

-- | The RHS of an NBNF rule
data Expression =
    Choice [Expression]                                 -- ^ e.g. @<expr> | <expr> | <expr>@
  | Sequence [Expression]                               -- ^ e.g. @<expr> <expr> <expr>@
  | Option Expression                                   -- ^ e.g. @[ <expr> ]@
  | Repetition Expression                               -- ^ e.g. @{ <expr> }@
  | Terminal TypedString                                -- ^ e.g. @"foo"@ or @'bar'@
  | NonTerminal Identifier                              -- ^ e.g. @<foo>@
  | Alphabet Identifier                                 -- ^ e.g. @<<Alphabet>>@
  | CharException Identifier [TypedChar]                -- ^ e.g. @<<Alphabet>> \ {"a", "b", 'c'}@
  | StringException Identifier Operator [TypedString]   -- ^ e.g. @<<Alphabet>>+ \ {"foo", "bar", 'baz'}@
  deriving (Eq)


-- | A character together with a string type
data TypedChar = TypedChar StringType Char deriving (Eq)

-- | A string together with a string type
data TypedString = TypedString StringType String deriving (Eq)

-- | Types of strings
data StringType = CaseSensitive | CaseInsensitive deriving (Eq)

-- | Operators to build a language from an alphabet
data Operator =
    Star    -- ^ All words over a given alphabet
  | Plus    -- ^ All non-empty words over a given alphabet
  deriving (Eq)

-- | All characters from the first character to the second in the Unicode rable
data CharSelection =
    CharChoice Char Char  -- ^ e.g. @"a" | ... | "z"@
  | SingleChar Char       -- ^ e.g. @"a"@ or @"\U+004A;"@
  deriving (Eq)
