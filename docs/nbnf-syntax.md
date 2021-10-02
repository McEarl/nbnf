## The syntax of NBNF

Here is a representation of the syntax of an NBNF rule which is accepted by
`nbnf-exe` in BNF:

```bnf
<NBNF rule> = <non-terminal rule>
            | <alphabet rule>

<non-terminal rule> = [ <spaces> ] "<" <identifier> ">" [ <horizontal spaces> ] <rule separator> [ <horizontal spaces> ] <expression> { [ <horizontal spaces> ] <vertical space> [ <horizontal spaces> ] "|" [ <horizontal spaces> ] <expression> } [ <spaces> ]

<alphabet rule> = [ <spaces> ] "<" <identifier> ">" [ <horizontal spaces> ] <rule separator> [ <horizontal spaces> ] <character selection> { [ <spaces> ] "|" [ <spaces> ] <expression> } [ <spaces> ]

<spaces> = <space> { <space> }

<space> = any Unicode space character and the control characters \t, \n, \r, \f, \v

<line break> = [ "\r" ] "\n"

<horizontal spaces> = <horizontal space> { <horizontal space> }

<horizontal space> = any Unicode space character and the control characters \t, \f, \v

<identifier> = <letter> | <digit> | " " | "_" | "-" { <letter> | <digit> | " " | "_" | "-" }

<rule separator> = "::=" | ":=" | "=" | "<-" | "->"

<expression> = <choice>
             | <sequence>
             | <option>
             | <repetition>
             | <grouping>
             | <terminal>
             | <string exception>
             | <character exception>
             | <alphabet>
             | <non-terminal>

<character selection> = <character choice>
                      | <character>

<letter> = "a" | ... | "z"
         | "A" | ... | "Z"

<digit> = "0" | ... | "9"

<choice> = <expression> { [ <horizontal spaces> ] "|" [ <horizontal spaces> ] <expression> }

<sequence> = <choice> = <expression> { [ <horizontal spaces> ] <expression> }

<option> = "[" [ <horizontal spaces> ] <expression> [ <horizontal spaces> ] "]"

<repetition> = <option> = "{" [ <horizontal spaces> ] <expression> [ <horizontal spaces> ] "}"

<grouping> = <option> = "(" [ <horizontal spaces> ] <expression> [ <horizontal spaces> ] ")"

<terminal> = <case-sensitive string>
         | <case-insensitive string>

<string exception> = <alphabet> <operator> [ <horizontal spaces> ] "\\" [ <horizontal spaces> ] "{" [ <horizontal spaces> ] <string list> [ <horizontal spaces> ] "}"

<character exception> = <alphabet> <operator> [ <horizontal spaces> ] "\\" [ <horizontal spaces> ] "{" [ <horizontal spaces> ] <character list> [ <horizontal spaces> ] "}"

<alphabet> = "<<" <identifier> ">>"

<non-terminal> = "<" <identifier> ">"

<character choice> = <character> [ <horizontal spaces> ] "|" [ <horizontal spaces> ] "..." [ <horizontal spaces> ] "|" [ <horizontal spaces> ] <character>

<character> = "\"" <char> "\""

<case-sensitive string> = "\"" <string> "\""

<case-insensitive string> = "\'" <string> "\'"

<string list> = <terminal> { [ <horizontal spaces> ] "," [ <horizontal spaces> ] <terminal> }

<character list> = <character> { [ <horizontal spaces> ] "," [ <horizontal spaces> ] <character> }

<string> = { <char> }

<char> = <unicode char>
       | <escaped char>
       | <regular char>

<unicode char> = "\\U+" <hex digits> ";"

<escaped char> = "\\" ( "\\" | "\"" | "\'" )

<regular char> = Any Unicode character except ", ', \

<hex digits> = <hex digit> { <hex digit> }

<hex digit> = "0" | ... | "9"
            | "a" | ... | "f"
            | "A" | ... | "F"
```

Note that the rule `<horizontal space>` does not really match only horizontal
spaces, but also kinds of vertical spaces. The point is that it does not match
any character involved in the rule `<line break>`.
