-- |
-- Module      : Grammar.NBNF
-- Copyright   : (c) 2021 Marcel Schütz
-- License     : MIT
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


-- | A character together with a string type
data TypedChar = TypedChar StringType Char

-- | A string together with a string type
data TypedString = TypedString StringType String

-- | Types of strings
data StringType = CaseSensitive | CaseInsensitive

-- | Operators to build a language from an alphabet
data Operator =
    Star    -- ^ All words over a given alphabet
  | Plus    -- ^ All non-empty words over a given alphabet

-- | All characters from the first character to the second in the Unicode rable
data CharSelection =
    CharChoice Char Char    -- ^ e.g. @"a" | ... | "z"@
  | SingleChar Char     -- ^ e.g. @"a"@ or @"\U+004A;"@


{-
-- * Utility functions

-- | Converts all occurences of given non-terminals on the rhs of each rule into
--   terminals with the same identifiers, e.g. @<foo>@ becomes @"foo"@. This
--   can be useful to prevent a grammar to be unfolded too deeply.
nonTerminalsToStrings :: Grammar -> [Identifier] -> Grammar
nonTerminalsToStrings grammar identifiers = map replaceRule (rules grammar)
  where
    replaceRule rule = rule{rhs = replaceExpr (rhs rule)}

    replaceExpr (Choice expressions) = Choice $ map replaceExpr expressions
    replaceExpr (Sequence expressions) = Sequence $ map replaceExpr expressions
    replaceExpr (Option expression) = Option $ map replaceExpr expression
    replaceExpr (Repetition expression) = Repetition $ map replaceExpr expression
    replaceExpr expr@(NonTerminal identifier) = if identifier `elem` identifiers
      then String CaseSensitive identifier
      else expr
    replaceExpr expr@(String _ _) = expr

-- | Take a grammar and a rule and generate the grammar we get by setting this
--   rule as the grammar's root. All rules which are superfluous after this
--   change are removed then. If the specified rule does not occur in the
--   grammar then 'Nothing' is returned.
generateSubgrammar :: Grammar -> Rule -> String -> Maybe Grammar
generateSubgrammar grammar newRoot newName = if newRoot `elem` rules grammar
  then Just $ Grammar newName newRules (lhs newRoot)
  else Nothing
    where
      newRules = collectRules grammar (collectNonTermIds (lhs newRoot))
      -- TODO: Currently only the rules of non-terminals in the new root's RHS
      --       are collected, but not the rules of their non-terminals!

-- Take a grammar and a list of identifiers and return a list of all rules in
-- this grammar whose identifiers occur in the given list.
collectRules :: Grammar -> [Identifier] -> [Rules]
collectRules _ [] = []
collectRules grammar (identifier : rest) =
  findRules identifier (rules grammar) ++ collectRules grammar rest
    where
      findRules _ [] = []
      findRules i (rule : rest) = if i == lhs rule
        then rule : findRules i rest
        else findRules i rest

-- Collect the identifiers of all non-terminals in an expression.
collectNonTermIds :: Expression -> [Identifier]
collectNonTermIds = nub . collectIds
  where
    collectIds expr = case expr of
      Choice expressions -> concatMap collectNonTermIds expressions
      Sequence expressions -> concatMap terminalsFromExpression expressions
      Option expression -> terminalsFromExpression expression
      Repetition expression -> terminalsFromExpression expression
      NonTerminal identifier -> [identifier]
      Terminal _ -> []
      String _ _ -> []
-}
