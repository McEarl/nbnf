import System.Console.ANSI
import System.Exit

import Import.Token
import Import.Parse

import Grammar.NBNF

import Export.LOG


-- Indent every line of a given string by two white spaces
indent :: String -> String
indent = unlines . map ("  " ++) . lines

-- Test whether the parse result of a given string matches a given rule
testParsing :: String   -- Name of the rule to be tested
            -> String   -- String whith is parsed
            -> Rule     -- Rule with is the expected parsing result
            -> Bool     -- Result (success/failure) of the last test iteration
            -> IO Bool  -- Result of the parsing test
testParsing name string rule previousResult = do
  putStr $ "\nTesting \'" ++ name ++ "\':\n"
  putStr $ indent string
  case parseNbnfToken (newNBNFTestToken string) of
    Left err -> do
      putStr "Parsing failed "
      putStr $ show err ++ "\n"
      setSGR [SetColor Foreground Vivid Red]
      putStr "Test failed!\n"
      setSGR [Reset]
      return False
    Right result -> do
      putStr $ "Parsed to:\n" ++ indent (ruleToLog result)
      if result == rule
      then do
        setSGR [SetColor Foreground Vivid Green]
        putStr "Test passed!\n"
        setSGR [Reset]
        return previousResult
      else do
        putStr $ "String does not match expected rule:\n" ++
          indent (ruleToLog rule)
        setSGR [SetColor Foreground Vivid Red]
        putStr "Test failed!\n"
        setSGR [Reset]
        return False

main :: IO ()
main = do
  result <- testParsing "Choice with inner sequences"
            "<identifier> = <foo1> <foo2> | <bar1> <bar2> | <baz1> <baz2>"
            (NonTerminalRule "identifier" (
              Choice [
                Sequence [
                  NonTerminal "foo1",
                  NonTerminal "foo2"
                ],
                Sequence [
                  NonTerminal "bar1",
                  NonTerminal "bar2"
                ],
                Sequence [
                  NonTerminal "baz1",
                  NonTerminal "baz2"
                ]
              ]
            ))
            True
        >>= testParsing "Simple sequence"
            "<identifier> = <foo> <bar> <baz>"
            (NonTerminalRule "identifier" (
              Sequence [
                NonTerminal "foo",
                NonTerminal "bar",
                NonTerminal "baz"
              ]
            ))
        >>= testParsing "Nested sequence"
            "<identifier> = <foo> ( <bar> ( <baz1> <baz2> ) )"
            (NonTerminalRule "identifier" (
              Sequence [
                NonTerminal "foo",
                NonTerminal "bar",
                NonTerminal "baz1",
                NonTerminal "baz2"
              ]
            ))
        >>= testParsing "Complex sequence"
            "<identifier> = { <foo> } ( <bar1> <bar2> ) [ <baz> ] ( <bang1> | <bang2> )"
            (NonTerminalRule "identifier" (
              Sequence [
                Repetition (
                  NonTerminal "foo"
                ),
                Sequence [
                  NonTerminal "bar1",
                  NonTerminal "bar2"
                ],
                Option (
                  NonTerminal "baz"
                ),
                Choice [
                  NonTerminal "bang1",
                  NonTerminal "bang2"
                ]
              ]
            ))
        >>= testParsing "Option"
            "<identifier> = [ <foo> ]"
            (NonTerminalRule "identifier" (
              Option (
                NonTerminal "foo"
              )
            ))
        >>= testParsing "Repetition"
            "<identifier> = { <foo> }"
            (NonTerminalRule "identifier" (
              Repetition (
                NonTerminal "foo"
              )
            ))
        >>= testParsing "Grouping"
            "<identifier> = ( <foo> )"
            (NonTerminalRule "identifier" (
              NonTerminal "foo"
            ))
        >>= testParsing "Case-sensitive string"
            "<identifier> = \"foo\\\"\\\'\""
            (NonTerminalRule "identifier" (
              Terminal (
                TypedString CaseSensitive "foo\"\'"
              )
            ))
        >>= testParsing "Case-insensitive string"
            "<identifier> = \'foo\\\"\\\'\'"
            (NonTerminalRule "identifier" (
            Terminal (
              TypedString CaseInsensitive "foo\"\'"
            )
            ))
        >>= testParsing "Alphabet"
            "<identifier> = <<foo>>"
            (NonTerminalRule "identifier" (
              Alphabet "foo"
            ))
        >>= testParsing "Character exception"
            "<identifier> = <<alphabet>> \\ { \"x\", \'y\' }"
            (NonTerminalRule "identifier" (
              CharException "alphabet" [
                TypedChar CaseSensitive 'x',
                TypedChar CaseInsensitive 'y'
              ]
            ))
        >>= testParsing "String exception (*)"
            "<identifier> = <<alphabet>>* \\ { \"foo\", \'bar\' }"
            (NonTerminalRule "identifier" (
              StringException "alphabet" Star [
                TypedString CaseSensitive "foo",
                TypedString CaseInsensitive "bar"
              ]
            ))
        >>= testParsing "String exception (+)"
            "<identifier> = <<alphabet>>+ \\ { \"foo\", \'bar\' }"
            (NonTerminalRule "identifier" (
              StringException "alphabet" Plus [
                TypedString CaseSensitive "foo",
                TypedString CaseInsensitive "bar"
              ]
            ))
        >>= testParsing "Singleton alphabet"
            "<<identifier>> = \"c\""
            (AlphabetRule "identifier" [
                SingleChar 'c'
              ]
            )
        >>= testParsing "Simple alphabet"
            "<<identifier>> = \"a\" | ... | \"z\""
            (AlphabetRule "identifier" [
                CharChoice 'a' 'z'
              ]
            )
        >>= testParsing "Complex alphabet"
            "<<identifier>> = \"a\" | ... | \"z\" | \"\\\"\" | \"0\" | ... | \"\\U+0039;\""
            (AlphabetRule "identifier" [
                CharChoice 'a' 'z',
                SingleChar '\"',
                CharChoice '0' '9'
              ]
            )

  if result then exitSuccess else exitFailure
