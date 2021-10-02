---
title: ForTheL tokens
---

This example describes the kinds of tokens a text written in the
[controlled natural language][1] ForTheL can consist of. The full specification
of ForTheL, which is also written in NBNF, can be found [here][2].


# Tokens

```nbnf
<ftl tokens> = { <alphanumeric token> | <symbolic token> | <white space> | <comment> }
```

```nbnf
<white space> = <<Horizontal Space>> | <<Vertical Space>>
```

```nbnf
<comment> = "#" { <<Visible Character>> | <<Horizontal Space>> } <<Vertical Space>>
```

```nbnf
<alphanumeric token> = <<Alphanum>> { <<Alphanum>> }
```

```nbnf
<symbolic token> = <<Symbol>> \ { "#" }
```


# Alphabets


```nbnf
<<Alphanum>> = "0" | ... | "9"
             | "A" | ... | "Z"
             | "a" | ... | "z"
```

```nbnf
<<Symbol>> = "\U+0021;" | ... | "\U+002F;"
           | "\U+003A;" | ... | "\U+0040;"
           | "\U+005B;" | ... | "\U+0060;"
           | "\U+007B;" | ... | "\U+007E;"
```

A symbol is a non-alphanumeric, printable character from Unicode's Basic Latin
block.

```nbnf
<<Horizontal Space>> = "\U+0009;"
                     | "\U+0020;"
```

A Horizontal space is either a horizontal tab or a space.

```nbnf
<<Vertical Space>> = "\U+000A;"
                   | "\U+000B;"
                   | "\U+000C;"
                   | "\U+000D;"
```

A vertical tab is a Line feed, vertical tab, form feed or carriage return.

```nbnf
<<Visible Character>> = "\U+0021;" | ... | "\U+007E;"
```

A visible character is a printable character from Unicode's Basic Latin block
which is neither a horizontal nor a vertical space.


[1]: <https://en.wikipedia.org/wiki/Controlled_natural_language>
[2]: <https://github.com/McEarl/forthel-syntax>
