---
title: First-order formulas
---

A primitive grammar which describes first-order formulas. It accepts for
instance the following strings:

  * "R(x, f(y, c)) and it is wrong that S(z)."
  * "For all x: If R(x) then (R'(x) or there is some y such that R''(y))."
  * "S(x, f(x)) iff there exists some y such that T(y, c)."

Note that a grammar in NBNF does not specify whether a parser for the language
it accepts is intended to take strings as input or lists of lexemes of a certain
form or something completely different. Hence we can assume that the grammar in
this example accepts the strings above although it does not specify the
behaviour of white spaces.


# Statements and formulas

```nbnf
<statement> = <formula> "."
```

```nbnf
<formula> = <biimplication>
          | <implication>
          | <conjunction>
          | <disjunction>
          | <negation>
          | <univeral formula>
          | <existential formula>
          | <atomic formula>
          | "(" <formula> ")"
```

```nbnf
<biimplication> = <formula> ('iff' | 'if' 'and' 'only' 'if') <formula>
```

```nbnf
<implication> = 'if' <formula> 'then' <formula>
```

```nbnf
<conjunction> = <formula> 'and' <formula>
```

```nbnf
<disjunction> = <formula> 'or' <formula>
```

```nbnf
<negation> = ( 'not' | 'it' 'is' 'wrong' 'that' ) <formula>
```

```nbnf
<universal formula> = 'for' ( 'all' | 'every' | 'any' ) <variable> ( ":" | 'we' 'have' ) <formula>
```

```nbnf
<existential statement> = 'there' ( 'is' | 'exists' ) 'some' <variable> 'such' 'that' <formula>
```


# Atomic formulas and terms

```nbnf
<atomic formula> = <<relation symbol>> "(" <term> { "," <term> } ")"
```

```nbnf
<term> = <variable>
       | <constant symbol>
       | <function symbol> "(" <term> { "," <term> } ")"
```


# Relation, function and constant symbols and variables

```nbnf
<relation symbol> = <<REL>> { "\'" }
```

```nbnf
<function symbol> = <<FCT>> { "\'" }
```

```nbnf
<constant symbol> = <<CONST>> { "\'" }
```

```nbnf
<variable> = <<VAR>> { "\'" }
```


# Alphabets

```nbnf
<<REL>> = "R" | "S" | "T"
```

```nbnf
<<FCT>> = "f" | "g" | "h"
```

```nbnf
<<CONST>> = "c"
```

```nbnf
<<VAR>> = "x" | "y" | "z"
```
